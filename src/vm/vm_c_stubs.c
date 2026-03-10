/*
   C dispatch loop for Vult bytecode VM with NaN-boxing.
   Full implementation matching the OCaml VM in exec.ml.
*/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* ========== NaN-boxing value representation ========== */

typedef uint64_t vm_val;

#define QNAN       0x7FFC000000000000ULL
#define TAG_MASK   0xFFFF000000000000ULL
#define TAG_VOID   0x7FFC000000000000ULL
#define TAG_INT    0x7FFD000000000000ULL
#define TAG_BOOL   0x7FFE000000000000ULL
#define TAG_STRUCT 0xFFFC000000000000ULL
#define TAG_ARRAY  0xFFFD000000000000ULL
#define TAG_STRING 0xFFFE000000000000ULL
#define TAG_LIST   0x7FFF000000000000ULL
#define PTR_MASK   0x0000FFFFFFFFFFFFULL

#define VM_VOID    TAG_VOID

static inline int is_real(vm_val v) { return (v & QNAN) != QNAN; }

static inline double as_real(vm_val v) {
    union { uint64_t u; double d; } c; c.u = v; return c.d;
}

static inline vm_val from_real(double d) {
    union { double d; uint64_t u; } c;
    c.d = d;
    if (__builtin_expect((c.u & QNAN) == QNAN, 0))
        c.u = 0x7FF8000000000000ULL; /* canonical NaN */
    return c.u;
}

static inline vm_val from_int(int32_t i) {
    return TAG_INT | ((uint64_t)(uint32_t)i);
}

static inline int32_t as_int(vm_val v) {
    return (int32_t)(uint32_t)(v & 0xFFFFFFFF);
}

static inline vm_val from_bool(int b) {
    return TAG_BOOL | (uint64_t)(b ? 1 : 0);
}

static inline int as_bool(vm_val v) {
    return (int)(v & 1);
}

/* ========== Compound value objects ========== */

typedef struct {
    int type;   /* 0 = struct, 1 = array */
    int length;
    vm_val fields[];
} vm_obj;

static inline vm_val from_struct(vm_obj *p) {
    return TAG_STRUCT | ((uint64_t)(uintptr_t)p & PTR_MASK);
}

static inline vm_val from_array_obj(vm_obj *p) {
    return TAG_ARRAY | ((uint64_t)(uintptr_t)p & PTR_MASK);
}

static inline vm_obj* as_obj(vm_val v) {
    return (vm_obj*)(uintptr_t)(v & PTR_MASK);
}

/* ========== String values ========== */

typedef struct {
    int length;
    char data[];
} vm_string;

static inline vm_val from_string(vm_string *s) {
    return TAG_STRING | ((uint64_t)(uintptr_t)s & PTR_MASK);
}

static inline vm_string* as_string(vm_val v) {
    return (vm_string*)(uintptr_t)(v & PTR_MASK);
}

/* ========== List values (dynamic array) ========== */

typedef struct {
    int length;
    int capacity;
    vm_val *items;
} vm_list;

static inline vm_val from_list(vm_list *l) {
    return TAG_LIST | ((uint64_t)(uintptr_t)l & PTR_MASK);
}

static inline vm_list* as_list(vm_val v) {
    return (vm_list*)(uintptr_t)(v & PTR_MASK);
}

/* ========== Arena allocator ========== */

#define ARENA_BLOCK_SIZE (1 << 20)

typedef struct arena_block {
    struct arena_block *next;
    size_t size;
    size_t pos;
    char data[];
} arena_block;

typedef struct {
    arena_block *current;
    arena_block *head;
} arena;

static arena_block* arena_new_block(size_t min_size) {
    size_t size = min_size > ARENA_BLOCK_SIZE ? min_size : ARENA_BLOCK_SIZE;
    arena_block *b = (arena_block*)malloc(sizeof(arena_block) + size);
    b->next = NULL;
    b->size = size;
    b->pos = 0;
    return b;
}

static void* arena_alloc(arena *a, size_t size) {
    size = (size + 7) & ~7;
    if (!a->current || a->current->pos + size > a->current->size) {
        arena_block *b = arena_new_block(size);
        if (a->head) b->next = a->head;
        a->head = b;
        a->current = b;
    }
    void *p = a->current->data + a->current->pos;
    a->current->pos += size;
    return p;
}

static void arena_free(arena *a) {
    arena_block *b = a->head;
    while (b) {
        arena_block *next = b->next;
        free(b);
        b = next;
    }
    a->head = NULL;
    a->current = NULL;
}

static vm_obj* alloc_obj(arena *a, int type, int length) {
    vm_obj *obj = (vm_obj*)arena_alloc(a, sizeof(vm_obj) + length * sizeof(vm_val));
    obj->type = type;
    obj->length = length;
    return obj;
}

static vm_string* alloc_string(arena *a, const char *data, int length) {
    vm_string *s = (vm_string*)arena_alloc(a, sizeof(vm_string) + length + 1);
    s->length = length;
    memcpy(s->data, data, length);
    s->data[length] = '\0';
    return s;
}

static vm_list* alloc_list(arena *a, int initial_capacity) {
    vm_list *l = (vm_list*)arena_alloc(a, sizeof(vm_list));
    l->length = 0;
    l->capacity = initial_capacity < 4 ? 4 : initial_capacity;
    l->items = (vm_val*)arena_alloc(a, l->capacity * sizeof(vm_val));
    return l;
}

static void list_ensure_capacity(arena *a, vm_list *l, int needed) {
    if (needed <= l->capacity) return;
    int new_cap = l->capacity * 2;
    if (new_cap < needed) new_cap = needed;
    vm_val *new_items = (vm_val*)arena_alloc(a, new_cap * sizeof(vm_val));
    memcpy(new_items, l->items, l->length * sizeof(vm_val));
    l->items = new_items;
    l->capacity = new_cap;
}

/* ========== C VM program ========== */

typedef struct {
    int32_t *code;
    int code_len;
    vm_val *constants;
    int n_constants;
    int *func_entry_pcs;
    int *func_n_args;
    int *func_n_locals;
    int n_functions;
    arena obj_arena;
} c_vm_prog;

/* ========== OCaml value -> vm_val conversion ========== */
/* Bytecode.value tags (non-constant constructors):
   Int=0, Int16=1, Real=2, Bool=3, String=4, Array=5, List=6, Struct=7
   Void = constant constructor 0 = Val_int(0) */

static vm_val ocaml_to_vm_val(arena *a, value v) {
    if (Is_long(v)) return VM_VOID;
    switch (Tag_val(v)) {
    case 0: /* Int */
        return from_int((int32_t)Long_val(Field(v, 0)));
    case 1: /* Int16 - store as int */
        return from_int((int32_t)Long_val(Field(v, 0)));
    case 2: /* Real */
        return from_real(Double_val(Field(v, 0)));
    case 3: /* Bool */
        return from_bool(Long_val(Field(v, 0)) != 0);
    case 4: { /* String */
        value s = Field(v, 0);
        int len = caml_string_length(s);
        vm_string *vs = alloc_string(a, String_val(s), len);
        return from_string(vs);
    }
    case 5: { /* Array */
        value arr = Field(v, 0);
        int len = Wosize_val(arr);
        vm_obj *obj = alloc_obj(a, 1, len);
        for (int i = 0; i < len; i++)
            obj->fields[i] = ocaml_to_vm_val(a, Field(arr, i));
        return from_array_obj(obj);
    }
    case 7: { /* Struct */
        value arr = Field(v, 0);
        int len = Wosize_val(arr);
        vm_obj *obj = alloc_obj(a, 0, len);
        for (int i = 0; i < len; i++)
            obj->fields[i] = ocaml_to_vm_val(a, Field(arr, i));
        return from_struct(obj);
    }
    case 6: { /* List of value list ref */
        value ref_block = Field(v, 0);
        value ocaml_list = Field(ref_block, 0);
        /* Count elements */
        int len = 0;
        value tmp = ocaml_list;
        while (tmp != Val_emptylist) { len++; tmp = Field(tmp, 1); }
        vm_list *vl = alloc_list(a, len > 4 ? len : 4);
        tmp = ocaml_list;
        for (int i = 0; i < len; i++) {
            vl->items[i] = ocaml_to_vm_val(a, Field(tmp, 0));
            tmp = Field(tmp, 1);
        }
        vl->length = len;
        return from_list(vl);
    }
    default:
        return VM_VOID;
    }
}

/* vm_val -> OCaml Bytecode.value */
static value vm_val_to_ocaml(vm_val v) {
    CAMLparam0();
    CAMLlocal4(result, arr, field_v, tmp);

    if (v == VM_VOID) CAMLreturn(Val_int(0));

    if (is_real(v)) {
        result = caml_alloc(1, 2);
        Store_field(result, 0, caml_copy_double(as_real(v)));
        CAMLreturn(result);
    }

    switch (v & TAG_MASK) {
    case TAG_INT:
        result = caml_alloc(1, 0);
        Store_field(result, 0, Val_long((long)as_int(v)));
        CAMLreturn(result);
    case TAG_BOOL:
        result = caml_alloc(1, 3);
        Store_field(result, 0, Val_long(as_bool(v) ? 1 : 0));
        CAMLreturn(result);
    case TAG_STRING: {
        vm_string *s = as_string(v);
        arr = caml_copy_string(s->data);
        result = caml_alloc(1, 4);
        Store_field(result, 0, arr);
        CAMLreturn(result);
    }
    case TAG_STRUCT: {
        vm_obj *obj = as_obj(v);
        arr = caml_alloc(obj->length, 0);
        for (int i = 0; i < obj->length; i++) {
            field_v = vm_val_to_ocaml(obj->fields[i]);
            Store_field(arr, i, field_v);
        }
        result = caml_alloc(1, 7);
        Store_field(result, 0, arr);
        CAMLreturn(result);
    }
    case TAG_ARRAY: {
        vm_obj *obj = as_obj(v);
        arr = caml_alloc(obj->length, 0);
        for (int i = 0; i < obj->length; i++) {
            field_v = vm_val_to_ocaml(obj->fields[i]);
            Store_field(arr, i, field_v);
        }
        result = caml_alloc(1, 5);
        Store_field(result, 0, arr);
        CAMLreturn(result);
    }
    case TAG_LIST: {
        vm_list *l = as_list(v);
        /* Build OCaml list from back to front */
        tmp = Val_emptylist;
        for (int i = l->length - 1; i >= 0; i--) {
            field_v = vm_val_to_ocaml(l->items[i]);
            arr = caml_alloc(2, 0); /* cons cell */
            Store_field(arr, 0, field_v);
            Store_field(arr, 1, tmp);
            tmp = arr;
        }
        /* Wrap in ref: List of value list ref */
        arr = caml_alloc(1, 0); /* ref block */
        Store_field(arr, 0, tmp);
        result = caml_alloc(1, 6); /* tag 6 = List */
        Store_field(result, 0, arr);
        CAMLreturn(result);
    }
    default:
        CAMLreturn(Val_int(0));
    }
}

/* ========== Value printing (for string builtin) ========== */

static void print_vm_val_buf(char *buf, int *pos, int max, vm_val v);

static void print_vm_val_buf(char *buf, int *pos, int max, vm_val v) {
    int p = *pos;
    if (v == VM_VOID) {
        p += snprintf(buf + p, max - p, "void");
    } else if (is_real(v)) {
        p += snprintf(buf + p, max - p, "%g", as_real(v));
    } else {
        switch (v & TAG_MASK) {
        case TAG_INT:
            p += snprintf(buf + p, max - p, "%d", (int)as_int(v));
            break;
        case TAG_BOOL:
            p += snprintf(buf + p, max - p, "%s", as_bool(v) ? "true" : "false");
            break;
        case TAG_STRING: {
            vm_string *s = as_string(v);
            p += snprintf(buf + p, max - p, "\"%s\"", s->data);
            break;
        }
        case TAG_STRUCT: {
            vm_obj *obj = as_obj(v);
            p += snprintf(buf + p, max - p, "{");
            for (int i = 0; i < obj->length && p < max - 10; i++) {
                if (i > 0) p += snprintf(buf + p, max - p, "; ");
                p += snprintf(buf + p, max - p, "%d:", i);
                *pos = p;
                print_vm_val_buf(buf, pos, max, obj->fields[i]);
                p = *pos;
            }
            p += snprintf(buf + p, max - p, "}");
            break;
        }
        case TAG_ARRAY: {
            vm_obj *obj = as_obj(v);
            p += snprintf(buf + p, max - p, "[");
            for (int i = 0; i < obj->length && p < max - 10; i++) {
                if (i > 0) p += snprintf(buf + p, max - p, "; ");
                *pos = p;
                print_vm_val_buf(buf, pos, max, obj->fields[i]);
                p = *pos;
            }
            p += snprintf(buf + p, max - p, "]");
            break;
        }
        case TAG_LIST: {
            vm_list *l = as_list(v);
            p += snprintf(buf + p, max - p, "list[");
            for (int i = 0; i < l->length && p < max - 10; i++) {
                if (i > 0) p += snprintf(buf + p, max - p, "; ");
                *pos = p;
                print_vm_val_buf(buf, pos, max, l->items[i]);
                p = *pos;
            }
            p += snprintf(buf + p, max - p, "]");
            break;
        }
        default:
            p += snprintf(buf + p, max - p, "?");
            break;
        }
    }
    if (p >= max) p = max - 1;
    *pos = p;
}

static vm_string* vm_val_to_string(arena *a, vm_val v) {
    char buf[4096];
    int pos = 0;
    print_vm_val_buf(buf, &pos, sizeof(buf), v);
    buf[pos] = '\0';
    return alloc_string(a, buf, pos);
}

/* ========== Builtin functions ========== */

static void exec_builtin(vm_val *stack, int *sp_ptr, arena *a, int id, double sample_rate) {
    int sp = *sp_ptr;
    switch (id) {
    case 0: { /* sin */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(sin(x));
        break;
    }
    case 1: { /* cos */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(cos(x));
        break;
    }
    case 2: { /* tan */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(tan(x));
        break;
    }
    case 3: { /* sinh */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(sinh(x));
        break;
    }
    case 4: { /* cosh */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(cosh(x));
        break;
    }
    case 5: { /* tanh */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(tanh(x));
        break;
    }
    case 6: { /* exp */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(exp(x));
        break;
    }
    case 7: { /* log */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(log(x));
        break;
    }
    case 8: { /* log10 */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(log10(x));
        break;
    }
    case 9: { /* sqrt */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(sqrt(x));
        break;
    }
    case 10: { /* abs */
        vm_val v = stack[--sp];
        if (is_real(v))
            stack[sp++] = from_real(fabs(as_real(v)));
        else
            stack[sp++] = from_int(abs(as_int(v)));
        break;
    }
    case 11: { /* floor */
        double x = as_real(stack[--sp]);
        stack[sp++] = from_real(floor(x));
        break;
    }
    case 12: { /* pow */
        double e = as_real(stack[--sp]);
        double b = as_real(stack[--sp]);
        stack[sp++] = from_real(pow(b, e));
        break;
    }
    case 13: { /* clip_real */
        double max_v = as_real(stack[--sp]);
        double min_v = as_real(stack[--sp]);
        double x = as_real(stack[--sp]);
        if (x < min_v) x = min_v;
        if (x > max_v) x = max_v;
        stack[sp++] = from_real(x);
        break;
    }
    case 14: { /* clip_int */
        int32_t max_v = as_int(stack[--sp]);
        int32_t min_v = as_int(stack[--sp]);
        int32_t x = as_int(stack[--sp]);
        if (x < min_v) x = min_v;
        if (x > max_v) x = max_v;
        stack[sp++] = from_int(x);
        break;
    }
    case 15: /* pi */
        stack[sp++] = from_real(3.14159265358979323846);
        break;
    case 16: /* eps */
        stack[sp++] = from_real(2.2204460492503131e-16);
        break;
    case 17: /* samplerate */
        stack[sp++] = from_real(sample_rate);
        break;
    case 18: /* random */
        stack[sp++] = from_real((double)rand() / (double)RAND_MAX);
        break;
    case 19: /* irandom */
        stack[sp++] = from_int((int32_t)rand());
        break;
    case 20: { /* real */
        vm_val v = stack[--sp];
        if (is_real(v)) { stack[sp++] = v; }
        else if ((v & TAG_MASK) == TAG_INT) { stack[sp++] = from_real((double)as_int(v)); }
        else if ((v & TAG_MASK) == TAG_BOOL) { stack[sp++] = from_real(as_bool(v) ? 1.0 : 0.0); }
        else { stack[sp++] = from_real(0.0); }
        break;
    }
    case 21: { /* int */
        vm_val v = stack[--sp];
        if ((v & TAG_MASK) == TAG_INT) { stack[sp++] = v; }
        else if (is_real(v)) { stack[sp++] = from_int((int32_t)as_real(v)); }
        else if ((v & TAG_MASK) == TAG_BOOL) { stack[sp++] = from_int(as_bool(v) ? 1 : 0); }
        else { stack[sp++] = from_int(0); }
        break;
    }
    case 22: { /* int16 - treat as int with clamping */
        vm_val v = stack[--sp];
        int32_t i;
        if ((v & TAG_MASK) == TAG_INT) { i = as_int(v); }
        else if (is_real(v)) { i = (int32_t)as_real(v); }
        else if ((v & TAG_MASK) == TAG_BOOL) { i = as_bool(v) ? 1 : 0; }
        else { i = 0; }
        if (i < -32768) i = -32768;
        if (i > 32767) i = 32767;
        stack[sp++] = from_int(i);
        break;
    }
    case 23: { /* bool */
        vm_val v = stack[--sp];
        if ((v & TAG_MASK) == TAG_BOOL) { stack[sp++] = v; }
        else if ((v & TAG_MASK) == TAG_INT) { stack[sp++] = from_bool(as_int(v) != 0); }
        else if (is_real(v)) { stack[sp++] = from_bool(as_real(v) != 0.0); }
        else { stack[sp++] = from_bool(0); }
        break;
    }
    case 25: { /* fix16 - treat as real */
        vm_val v = stack[--sp];
        if (is_real(v)) { stack[sp++] = v; }
        else if ((v & TAG_MASK) == TAG_INT) { stack[sp++] = from_real((double)as_int(v)); }
        else { stack[sp++] = from_real(0.0); }
        break;
    }
    case 24: { /* string */
        vm_val v = stack[--sp];
        vm_string *s = vm_val_to_string(a, v);
        stack[sp++] = from_string(s);
        break;
    }
    case 26: { /* size */
        vm_val v = stack[--sp];
        vm_obj *obj = as_obj(v);
        stack[sp++] = from_int(obj->length);
        break;
    }
    case 27: { /* length */
        vm_val v = stack[--sp];
        vm_string *s = as_string(v);
        stack[sp++] = from_int(s->length);
        break;
    }
    case 28: { /* list_size */
        vm_val v = stack[--sp];
        vm_list *l = as_list(v);
        stack[sp++] = from_int(l->length);
        break;
    }
    case 29: { /* list_capacity */
        vm_val v = stack[--sp];
        vm_list *l = as_list(v);
        stack[sp++] = from_int(l->length);
        break;
    }
    case 30: { /* list_append */
        vm_val elem = stack[--sp];
        vm_val lst = stack[--sp];
        vm_list *l = as_list(lst);
        list_ensure_capacity(a, l, l->length + 1);
        l->items[l->length++] = elem;
        stack[sp++] = VM_VOID;
        break;
    }
    case 31: { /* list_insert */
        vm_val elem = stack[--sp];
        vm_val idx_v = stack[--sp];
        vm_val lst = stack[--sp];
        vm_list *l = as_list(lst);
        int idx = as_int(idx_v);
        list_ensure_capacity(a, l, l->length + 1);
        memmove(&l->items[idx + 1], &l->items[idx], (l->length - idx) * sizeof(vm_val));
        l->items[idx] = elem;
        l->length++;
        stack[sp++] = VM_VOID;
        break;
    }
    case 32: { /* list_remove */
        vm_val idx_v = stack[--sp];
        vm_val lst = stack[--sp];
        vm_list *l = as_list(lst);
        int idx = as_int(idx_v);
        memmove(&l->items[idx], &l->items[idx + 1], (l->length - idx - 1) * sizeof(vm_val));
        l->length--;
        stack[sp++] = VM_VOID;
        break;
    }
    case 33: { /* list_clear */
        vm_val lst = stack[--sp];
        vm_list *l = as_list(lst);
        l->length = 0;
        stack[sp++] = VM_VOID;
        break;
    }
    case 34: { /* list_reserve */
        vm_val _n = stack[--sp];
        vm_val lst = stack[--sp];
        (void)lst; (void)_n;
        stack[sp++] = VM_VOID;
        break;
    }
    case 35: { /* list_get */
        vm_val idx_v = stack[--sp];
        vm_val lst = stack[--sp];
        vm_list *l = as_list(lst);
        int idx = as_int(idx_v);
        stack[sp++] = l->items[idx];
        break;
    }
    case 36: { /* list_set */
        vm_val elem = stack[--sp];
        vm_val idx_v = stack[--sp];
        vm_val lst = stack[--sp];
        vm_list *l = as_list(lst);
        int idx = as_int(idx_v);
        l->items[idx] = elem;
        stack[sp++] = VM_VOID;
        break;
    }
    default:
        fprintf(stderr, "C VM: unsupported builtin id %d\n", id);
        stack[sp++] = VM_VOID;
        break;
    }
    *sp_ptr = sp;
}

/* ========== Binary operations ========== */

static void exec_binop(vm_val *stack, int *sp_ptr, int tag) {
    int sp = *sp_ptr;
    vm_val v2 = stack[--sp];
    vm_val v1 = stack[--sp];

    switch (tag) {
    case 0: /* Le <= */
        if (is_real(v1) && is_real(v2))
            stack[sp++] = from_bool(as_real(v1) <= as_real(v2));
        else
            stack[sp++] = from_bool(as_int(v1) <= as_int(v2));
        break;
    case 1: /* Ge >= */
        if (is_real(v1) && is_real(v2))
            stack[sp++] = from_bool(as_real(v1) >= as_real(v2));
        else
            stack[sp++] = from_bool(as_int(v1) >= as_int(v2));
        break;
    case 2: /* Ne <> */
        if (is_real(v1) && is_real(v2))
            stack[sp++] = from_bool(as_real(v1) != as_real(v2));
        else if ((v1 & TAG_MASK) == TAG_BOOL)
            stack[sp++] = from_bool(as_bool(v1) != as_bool(v2));
        else
            stack[sp++] = from_bool(as_int(v1) != as_int(v2));
        break;
    case 3: /* Land && */
        stack[sp++] = from_bool(as_bool(v1) && as_bool(v2));
        break;
    case 4: /* Lor || */
        stack[sp++] = from_bool(as_bool(v1) || as_bool(v2));
        break;
    case 5: /* Band & */
        stack[sp++] = from_int(as_int(v1) & as_int(v2));
        break;
    case 6: /* Bor | */
        stack[sp++] = from_int(as_int(v1) | as_int(v2));
        break;
    case 7: /* Bxor ^ */
        stack[sp++] = from_int(as_int(v1) ^ as_int(v2));
        break;
    case 8: /* Lsh << */
        stack[sp++] = from_int(as_int(v1) << as_int(v2));
        break;
    case 9: /* Rsh >> */
        stack[sp++] = from_int(as_int(v1) >> as_int(v2));
        break;
    case 10: /* Mod % */
        if (is_real(v1) && is_real(v2))
            stack[sp++] = from_real(fmod(as_real(v1), as_real(v2)));
        else {
            int32_t b = as_int(v2);
            stack[sp++] = from_int(b != 0 ? as_int(v1) % b : 0);
        }
        break;
    default:
        stack[sp++] = VM_VOID;
        break;
    }
    *sp_ptr = sp;
}

/* ========== Main dispatch loop ========== */

static vm_val c_vm_run(c_vm_prog *prog, int func_idx,
                        vm_val *args, int n_call_args, double sample_rate) {
    int32_t *code = prog->code;
    int code_len = prog->code_len;
    vm_val *constants = prog->constants;
    int *func_entry_pcs = prog->func_entry_pcs;
    int *func_n_locals_tbl = prog->func_n_locals;
    arena *obj_arena = &prog->obj_arena;

    /* VM state - static to avoid stack overflow */
    static vm_val stack[65536];
    static vm_val locals[65536];
    static int cs_return_pcs[1024];
    static int cs_saved_fps[1024];
    static int cs_saved_sps[1024];
    static int cs_saved_locals_sps[1024];

    int pc, sp, fp, locals_sp, csp;

    /* Set up initial frame */
    int entry_pc = func_entry_pcs[func_idx];
    int n_locals = func_n_locals_tbl[func_idx];

    for (int i = 0; i < n_call_args; i++) locals[i] = args[i];
    for (int i = n_call_args; i < n_locals; i++) locals[i] = VM_VOID;

    pc = entry_pc;
    sp = 0;
    fp = 0;
    locals_sp = n_locals;
    csp = 0;

    for (;;) {
        if (__builtin_expect(pc >= code_len, 0))
            return sp > 0 ? stack[--sp] : VM_VOID;

        switch (code[pc]) {

        case 0: { /* LoadLocal */
            int idx = code[pc+1];
            stack[sp++] = locals[fp + idx];
            pc += 2;
            break;
        }
        case 1: { /* StoreLocal */
            int idx = code[pc+1];
            locals[fp + idx] = stack[--sp];
            pc += 2;
            break;
        }
        case 2: { /* Loadc */
            int idx = code[pc+1];
            stack[sp++] = constants[idx];
            pc += 2;
            break;
        }
        case 3: /* Pop */
            sp--;
            pc++;
            break;
        case 4: /* Dup */
            stack[sp] = stack[sp-1];
            sp++;
            pc++;
            break;
        case 5: { /* AddInt */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_int(a + b);
            pc++;
            break;
        }
        case 6: { /* SubInt */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_int(a - b);
            pc++;
            break;
        }
        case 7: { /* MulInt */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_int(a * b);
            pc++;
            break;
        }
        case 8: { /* DivInt */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_int(b != 0 ? a / b : 0);
            pc++;
            break;
        }
        case 9: case 10: case 11: case 12: /* Int16 arithmetic - treat as int */
        {
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            int32_t r;
            switch (code[pc]) {
                case 9:  r = a + b; break;
                case 10: r = a - b; break;
                case 11: r = a * b; break;
                case 12: r = (b != 0) ? a / b : 0; break;
                default: r = 0; break;
            }
            if (r < -32768) r = -32768;
            if (r > 32767) r = 32767;
            stack[sp++] = from_int(r);
            pc++;
            break;
        }
        case 13: { /* AddReal */
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            stack[sp++] = from_real(a + b);
            pc++;
            break;
        }
        case 14: { /* SubReal */
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            stack[sp++] = from_real(a - b);
            pc++;
            break;
        }
        case 15: { /* MulReal */
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            stack[sp++] = from_real(a * b);
            pc++;
            break;
        }
        case 16: { /* DivReal */
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            stack[sp++] = from_real(a / b);
            pc++;
            break;
        }
        case 17: { /* ModInt */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_int(b != 0 ? a % b : 0);
            pc++;
            break;
        }
        case 18: { /* ModInt16 */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_int(b != 0 ? a % b : 0);
            pc++;
            break;
        }
        case 19: { /* ModReal */
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            stack[sp++] = from_real(fmod(a, b));
            pc++;
            break;
        }
        case 20: { /* NegInt */
            stack[sp-1] = from_int(-as_int(stack[sp-1]));
            pc++;
            break;
        }
        case 21: { /* NegReal */
            stack[sp-1] = from_real(-as_real(stack[sp-1]));
            pc++;
            break;
        }
        case 22: { /* NegInt16 */
            int32_t v = -as_int(stack[sp-1]);
            if (v < -32768) v = -32768;
            if (v > 32767) v = 32767;
            stack[sp-1] = from_int(v);
            pc++;
            break;
        }
        case 23: { /* Not */
            stack[sp-1] = from_bool(!as_bool(stack[sp-1]));
            pc++;
            break;
        }
        case 24: { /* EqInt */
            vm_val b = stack[--sp];
            vm_val a = stack[--sp];
            if ((a & TAG_MASK) == TAG_BOOL)
                stack[sp++] = from_bool(as_bool(a) == as_bool(b));
            else
                stack[sp++] = from_bool(as_int(a) == as_int(b));
            pc++;
            break;
        }
        case 25: { /* EqInt16 */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_bool(a == b);
            pc++;
            break;
        }
        case 26: { /* EqReal */
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            stack[sp++] = from_bool(a == b);
            pc++;
            break;
        }
        case 27: { /* LtInt */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_bool(a < b);
            pc++;
            break;
        }
        case 28: { /* LtInt16 */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_bool(a < b);
            pc++;
            break;
        }
        case 29: { /* LtReal */
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            stack[sp++] = from_bool(a < b);
            pc++;
            break;
        }
        case 30: { /* GtInt */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_bool(a > b);
            pc++;
            break;
        }
        case 31: { /* GtInt16 */
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            stack[sp++] = from_bool(a > b);
            pc++;
            break;
        }
        case 32: { /* GtReal */
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            stack[sp++] = from_bool(a > b);
            pc++;
            break;
        }
        case 33: { /* BinOp */
            int tag = code[pc+1];
            pc += 2;
            exec_binop(stack, &sp, tag);
            break;
        }
        case 34: /* Jump */
            pc = code[pc+1];
            break;
        case 35: { /* JumpIfFalse */
            int target = code[pc+1];
            pc += 2;
            if (!as_bool(stack[--sp])) pc = target;
            break;
        }
        case 36: { /* JumpIfTrue */
            int target = code[pc+1];
            pc += 2;
            if (as_bool(stack[--sp])) pc = target;
            break;
        }
        case 37: /* Halt */
            return sp > 0 ? stack[--sp] : VM_VOID;

        case 38: { /* Call */
            int fi = code[pc+1];
            int nargs = code[pc+2];
            pc += 3;
            int nloc = func_n_locals_tbl[fi];
            cs_return_pcs[csp] = pc;
            cs_saved_fps[csp] = fp;
            cs_saved_sps[csp] = sp - nargs;
            cs_saved_locals_sps[csp] = locals_sp;
            csp++;
            int new_fp = locals_sp;
            for (int i = 0; i < nargs; i++)
                locals[new_fp + i] = stack[sp - nargs + i];
            sp -= nargs;
            fp = new_fp;
            locals_sp = new_fp + nloc;
            pc = func_entry_pcs[fi];
            break;
        }
        case 39: { /* Return */
            vm_val result = stack[--sp];
            if (csp <= 0) return result;
            csp--;
            pc = cs_return_pcs[csp];
            fp = cs_saved_fps[csp];
            sp = cs_saved_sps[csp];
            locals_sp = cs_saved_locals_sps[csp];
            stack[sp++] = result;
            break;
        }
        case 40: { /* ReturnVoid */
            if (csp <= 0) return VM_VOID;
            csp--;
            pc = cs_return_pcs[csp];
            fp = cs_saved_fps[csp];
            sp = cs_saved_sps[csp];
            locals_sp = cs_saved_locals_sps[csp];
            stack[sp++] = VM_VOID;
            break;
        }
        case 41: { /* CallBuiltin */
            int id = code[pc+1];
            /* int nargs = code[pc+2]; -- unused, builtin knows its arity */
            pc += 3;
            exec_builtin(stack, &sp, obj_arena, id, sample_rate);
            break;
        }
        case 42: { /* MakeArray */
            int n = code[pc+1];
            pc += 2;
            vm_obj *obj = alloc_obj(obj_arena, 1, n);
            for (int i = n-1; i >= 0; i--)
                obj->fields[i] = stack[--sp];
            stack[sp++] = from_array_obj(obj);
            break;
        }
        case 43: { /* MakeStruct */
            int n = code[pc+1];
            pc += 2;
            vm_obj *obj = alloc_obj(obj_arena, 0, n);
            for (int i = n-1; i >= 0; i--)
                obj->fields[i] = stack[--sp];
            stack[sp++] = from_struct(obj);
            break;
        }
        case 44: { /* MakeTuple - treat like array */
            int n = code[pc+1];
            pc += 2;
            vm_obj *obj = alloc_obj(obj_arena, 1, n);
            for (int i = n-1; i >= 0; i--)
                obj->fields[i] = stack[--sp];
            stack[sp++] = from_array_obj(obj);
            break;
        }
        case 45: { /* IndexLoad */
            vm_val idx_v = stack[--sp];
            vm_val arr_v = stack[--sp];
            int idx = as_int(idx_v);
            if ((arr_v & TAG_MASK) == TAG_LIST) {
                vm_list *l = as_list(arr_v);
                stack[sp++] = l->items[idx];
            } else {
                vm_obj *obj = as_obj(arr_v);
                stack[sp++] = obj->fields[idx];
            }
            pc++;
            break;
        }
        case 46: { /* IndexStore */
            vm_val arr_v = stack[--sp];
            vm_val idx_v = stack[--sp];
            vm_val v = stack[--sp];
            int idx = as_int(idx_v);
            if ((arr_v & TAG_MASK) == TAG_LIST) {
                vm_list *l = as_list(arr_v);
                l->items[idx] = v;
            } else {
                vm_obj *obj = as_obj(arr_v);
                obj->fields[idx] = v;
            }
            pc++;
            break;
        }
        case 47: { /* MemberLoad */
            int idx = code[pc+1];
            pc += 2;
            vm_val s = stack[--sp];
            vm_obj *obj = as_obj(s);
            stack[sp++] = obj->fields[idx];
            break;
        }
        case 48: { /* MemberStore */
            int idx = code[pc+1];
            pc += 2;
            vm_val s = stack[--sp];
            vm_val v = stack[--sp];
            vm_obj *obj = as_obj(s);
            obj->fields[idx] = v;
            break;
        }
        case 49: { /* UnpackTuple */
            int n = code[pc+1];
            vm_val tuple = stack[--sp];
            vm_obj *obj = as_obj(tuple);
            for (int i = 0; i < n && i < obj->length; i++) {
                int offset = code[pc + 2 + i];
                locals[fp + offset] = obj->fields[i];
            }
            pc += 2 + n;
            break;
        }
        case 50: { /* MakeRecord */
            /* int struct_idx = code[pc+1]; */
            int n = code[pc+2];
            pc += 3;
            vm_obj *obj = alloc_obj(obj_arena, 0, n);
            for (int i = n-1; i >= 0; i--)
                obj->fields[i] = stack[--sp];
            stack[sp++] = from_struct(obj);
            break;
        }
        case 51: { /* CallExternal - skip */
            /* int name_hash = code[pc+1]; */
            int nargs = code[pc+2];
            pc += 3;
            for (int i = 0; i < nargs; i++) sp--;
            stack[sp++] = VM_VOID;
            break;
        }
        case 52: { /* LoadLocalMember */
            int local_idx = code[pc+1];
            int member_idx = code[pc+2];
            pc += 3;
            vm_val s = locals[fp + local_idx];
            vm_obj *obj = as_obj(s);
            stack[sp++] = obj->fields[member_idx];
            break;
        }
        case 53: { /* StoreLocalMember */
            int local_idx = code[pc+1];
            int member_idx = code[pc+2];
            pc += 3;
            vm_val v = stack[--sp];
            vm_val s = locals[fp + local_idx];
            vm_obj *obj = as_obj(s);
            obj->fields[member_idx] = v;
            break;
        }
        case 54: { /* DupStoreLocal */
            int idx = code[pc+1];
            pc += 2;
            locals[fp + idx] = stack[sp-1];
            break;
        }
        case 55: { /* DupStoreLocalMember */
            int local_idx = code[pc+1];
            int member_idx = code[pc+2];
            pc += 3;
            vm_val v = stack[sp-1];
            vm_val s = locals[fp + local_idx];
            vm_obj *obj = as_obj(s);
            obj->fields[member_idx] = v;
            break;
        }

        /* Call0-3: specialized call opcodes */
        case 56: { /* Call0 */
            int fi = code[pc+1];
            pc += 2;
            cs_return_pcs[csp] = pc;
            cs_saved_fps[csp] = fp;
            cs_saved_sps[csp] = sp;
            cs_saved_locals_sps[csp] = locals_sp;
            csp++;
            int new_fp = locals_sp;
            fp = new_fp;
            locals_sp = new_fp + func_n_locals_tbl[fi];
            pc = func_entry_pcs[fi];
            break;
        }
        case 57: { /* Call1 */
            int fi = code[pc+1];
            pc += 2;
            cs_return_pcs[csp] = pc;
            cs_saved_fps[csp] = fp;
            cs_saved_sps[csp] = sp - 1;
            cs_saved_locals_sps[csp] = locals_sp;
            csp++;
            int new_fp = locals_sp;
            locals[new_fp] = stack[--sp];
            fp = new_fp;
            locals_sp = new_fp + func_n_locals_tbl[fi];
            pc = func_entry_pcs[fi];
            break;
        }
        case 58: { /* Call2 */
            int fi = code[pc+1];
            pc += 2;
            cs_return_pcs[csp] = pc;
            cs_saved_fps[csp] = fp;
            cs_saved_sps[csp] = sp - 2;
            cs_saved_locals_sps[csp] = locals_sp;
            csp++;
            int new_fp = locals_sp;
            locals[new_fp+1] = stack[--sp];
            locals[new_fp] = stack[--sp];
            fp = new_fp;
            locals_sp = new_fp + func_n_locals_tbl[fi];
            pc = func_entry_pcs[fi];
            break;
        }
        case 59: { /* Call3 */
            int fi = code[pc+1];
            pc += 2;
            cs_return_pcs[csp] = pc;
            cs_saved_fps[csp] = fp;
            cs_saved_sps[csp] = sp - 3;
            cs_saved_locals_sps[csp] = locals_sp;
            csp++;
            int new_fp = locals_sp;
            locals[new_fp+2] = stack[--sp];
            locals[new_fp+1] = stack[--sp];
            locals[new_fp] = stack[--sp];
            fp = new_fp;
            locals_sp = new_fp + func_n_locals_tbl[fi];
            pc = func_entry_pcs[fi];
            break;
        }

        /* Specialized LoadLocal 0-3 */
        case 60: stack[sp++] = locals[fp];     pc++; break;
        case 61: stack[sp++] = locals[fp + 1]; pc++; break;
        case 62: stack[sp++] = locals[fp + 2]; pc++; break;
        case 63: stack[sp++] = locals[fp + 3]; pc++; break;

        /* Specialized StoreLocal 0-3 */
        case 64: locals[fp]     = stack[--sp]; pc++; break;
        case 65: locals[fp + 1] = stack[--sp]; pc++; break;
        case 66: locals[fp + 2] = stack[--sp]; pc++; break;
        case 67: locals[fp + 3] = stack[--sp]; pc++; break;

        /* Specialized Loadc 0-3 */
        case 68: stack[sp++] = constants[0]; pc++; break;
        case 69: stack[sp++] = constants[1]; pc++; break;
        case 70: stack[sp++] = constants[2]; pc++; break;
        case 71: stack[sp++] = constants[3]; pc++; break;

        /* Specialized DupStoreLocal 0-3 */
        case 72: locals[fp]     = stack[sp-1]; pc++; break;
        case 73: locals[fp + 1] = stack[sp-1]; pc++; break;
        case 74: locals[fp + 2] = stack[sp-1]; pc++; break;
        case 75: locals[fp + 3] = stack[sp-1]; pc++; break;

        /* Fused compare+branch */
        case 76: { /* LtIntJumpIfFalse */
            int target = code[pc+1];
            pc += 2;
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            if (!(a < b)) pc = target;
            break;
        }
        case 77: { /* GtIntJumpIfFalse */
            int target = code[pc+1];
            pc += 2;
            int32_t b = as_int(stack[--sp]);
            int32_t a = as_int(stack[--sp]);
            if (!(a > b)) pc = target;
            break;
        }
        case 78: { /* EqIntJumpIfFalse */
            int target = code[pc+1];
            pc += 2;
            vm_val b = stack[--sp];
            vm_val a = stack[--sp];
            int eq;
            if ((a & TAG_MASK) == TAG_BOOL)
                eq = (as_bool(a) == as_bool(b));
            else
                eq = (as_int(a) == as_int(b));
            if (!eq) pc = target;
            break;
        }
        case 79: { /* LtRealJumpIfFalse */
            int target = code[pc+1];
            pc += 2;
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            if (!(a < b)) pc = target;
            break;
        }
        case 80: { /* GtRealJumpIfFalse */
            int target = code[pc+1];
            pc += 2;
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            if (!(a > b)) pc = target;
            break;
        }
        case 81: { /* EqRealJumpIfFalse */
            int target = code[pc+1];
            pc += 2;
            double b = as_real(stack[--sp]);
            double a = as_real(stack[--sp]);
            if (a != b) pc = target;
            break;
        }

        default:
            fprintf(stderr, "C VM: unknown opcode %d at pc=%d\n", code[pc], pc);
            return VM_VOID;
        }
    }
}

/* ========== Custom block for c_vm_prog ========== */

static void finalize_c_vm_prog(value v) {
    c_vm_prog *prog = *(c_vm_prog**)Data_custom_val(v);
    if (prog) {
        arena_free(&prog->obj_arena);
        free(prog->code);
        free(prog->constants);
        free(prog->func_entry_pcs);
        free(prog->func_n_args);
        free(prog->func_n_locals);
        free(prog);
    }
}

static struct custom_operations c_vm_prog_ops = {
    "vult.c_vm_prog",
    finalize_c_vm_prog,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* ========== OCaml stubs ========== */

CAMLprim value caml_c_prepare_program(value v_encoded, value v_constants,
                                       value v_entry_pcs, value v_n_args,
                                       value v_n_locals) {
    CAMLparam5(v_encoded, v_constants, v_entry_pcs, v_n_args, v_n_locals);
    CAMLlocal1(v_handle);

    c_vm_prog *prog = (c_vm_prog*)calloc(1, sizeof(c_vm_prog));

    /* Copy encoded bytecode */
    int code_len = Wosize_val(v_encoded);
    prog->code = (int32_t*)malloc(code_len * sizeof(int32_t));
    prog->code_len = code_len;
    for (int i = 0; i < code_len; i++)
        prog->code[i] = (int32_t)Long_val(Field(v_encoded, i));

    /* Convert constants */
    int n_constants = Wosize_val(v_constants);
    prog->constants = (vm_val*)malloc(n_constants * sizeof(vm_val));
    prog->n_constants = n_constants;
    for (int i = 0; i < n_constants; i++)
        prog->constants[i] = ocaml_to_vm_val(&prog->obj_arena, Field(v_constants, i));

    /* Copy function tables */
    int n_functions = Wosize_val(v_entry_pcs);
    prog->n_functions = n_functions;
    prog->func_entry_pcs = (int*)malloc(n_functions * sizeof(int));
    prog->func_n_args = (int*)malloc(n_functions * sizeof(int));
    prog->func_n_locals = (int*)malloc(n_functions * sizeof(int));
    for (int i = 0; i < n_functions; i++) {
        prog->func_entry_pcs[i] = (int)Long_val(Field(v_entry_pcs, i));
        prog->func_n_args[i] = (int)Long_val(Field(v_n_args, i));
        prog->func_n_locals[i] = (int)Long_val(Field(v_n_locals, i));
    }

    v_handle = caml_alloc_custom(&c_vm_prog_ops, sizeof(c_vm_prog*), 0, 1);
    *(c_vm_prog**)Data_custom_val(v_handle) = prog;

    CAMLreturn(v_handle);
}

CAMLprim value caml_c_run_function(value v_handle, value v_func_idx,
                                    value v_args, value v_sample_rate) {
    CAMLparam4(v_handle, v_func_idx, v_args, v_sample_rate);
    CAMLlocal1(v_result);

    c_vm_prog *prog = *(c_vm_prog**)Data_custom_val(v_handle);
    int func_idx = Int_val(v_func_idx);
    double sample_rate = Double_val(v_sample_rate);

    /* Convert args list to vm_val array */
    vm_val c_args[16];
    int n_args = 0;
    value list = v_args;
    while (list != Val_emptylist) {
        c_args[n_args++] = ocaml_to_vm_val(&prog->obj_arena, Field(list, 0));
        list = Field(list, 1);
        if (n_args >= 16) break;
    }

    vm_val result = c_vm_run(prog, func_idx, c_args, n_args, sample_rate);

    v_result = vm_val_to_ocaml(result);

    CAMLreturn(v_result);
}
