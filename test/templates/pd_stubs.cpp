/* Minimal Pure Data API stubs used to link and run the generated externals in the tests.
   Linking against these catches undefined symbols (e.g. a runtime function the template
   forgot to define), and running the setup plus one instantiation of every registered
   class exercises the registration and constructor paths. */
#include <m_pd.h>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

extern "C" {

t_symbol s_signal = {(char *)"signal", nullptr, nullptr};
t_symbol s_float = {(char *)"float", nullptr, nullptr};
t_symbol s_symbol = {(char *)"symbol", nullptr, nullptr};
t_symbol s_list = {(char *)"list", nullptr, nullptr};
t_symbol s_bang = {(char *)"bang", nullptr, nullptr};
t_symbol s_ = {(char *)"", nullptr, nullptr};

/* interned symbols with stable addresses */
t_symbol *gensym(const char *name) {
   struct entry { t_symbol sym; entry *next; };
   static entry *table = nullptr;
   for (entry *e = table; e != nullptr; e = e->next) {
      if (strcmp(e->sym.s_name, name) == 0) return &e->sym;
   }
   entry *e = (entry *)calloc(1, sizeof(entry));
   e->sym.s_name = strdup(name);
   e->next = table;
   table = e;
   return &e->sym;
}

struct _class {
   t_symbol *name;
   t_newmethod newmethod;
   size_t size;
   struct _class *next;
};

static t_class *class_registry = nullptr;

t_class *class_new(t_symbol *name, t_newmethod newmethod, t_method, size_t size, int, t_atomtype, ...) {
   t_class *c = (t_class *)calloc(1, sizeof(t_class));
   c->name = name;
   c->newmethod = newmethod;
   c->size = size;
   c->next = class_registry;
   class_registry = c;
   return c;
}

void class_addmethod(t_class *, t_method, t_symbol *, t_atomtype, ...) {}
#undef class_addbang
void class_addbang(t_class *, t_method) {}
void class_doaddfloat(t_class *, t_method) {}
#undef class_addsymbol
void class_addsymbol(t_class *, t_method) {}
#undef class_addlist
void class_addlist(t_class *, t_method) {}
#undef class_addanything
void class_addanything(t_class *, t_method) {}
void class_domainsignalin(t_class *, int) {}

t_pd *pd_new(t_class *c) { return (t_pd *)calloc(1, c->size); }

t_inlet *inlet_new(t_object *, t_pd *, t_symbol *, t_symbol *) { return (t_inlet *)calloc(1, 8); }
t_inlet *floatinlet_new(t_object *, t_float *) { return (t_inlet *)calloc(1, 8); }
t_inlet *symbolinlet_new(t_object *, t_symbol **) { return (t_inlet *)calloc(1, 8); }
t_outlet *outlet_new(t_object *, t_symbol *) { return (t_outlet *)calloc(1, 8); }
void outlet_float(t_outlet *, t_float) {}
void outlet_symbol(t_outlet *, t_symbol *) {}

void dsp_add(t_perfroutine, int, ...) {}
t_float sys_getsr(void) { return 44100; }

void pd_error(void *, const char *fmt, ...) {
   va_list args;
   va_start(args, fmt);
   vfprintf(stderr, fmt, args);
   va_end(args);
   fprintf(stderr, "\n");
}

void post(const char *fmt, ...) {
   va_list args;
   va_start(args, fmt);
   vfprintf(stdout, fmt, args);
   va_end(args);
   fprintf(stdout, "\n");
}

/* The setup entry of the library under test, selected at compile time. */
void VULT_TEST_SETUP(void);

} // extern "C"

int main() {
   VULT_TEST_SETUP();
   int classes = 0;
   for (t_class *c = class_registry; c != nullptr; c = c->next) {
      void *obj = ((void *(*)(void))c->newmethod)();
      if (obj == nullptr) {
         fprintf(stderr, "failed to create an instance of '%s'\n", c->name->s_name);
         return 1;
      }
      classes++;
   }
   if (classes == 0) {
      fprintf(stderr, "the library registered no classes\n");
      return 1;
   }
   printf("created %d objects\n", classes);
   return 0;
}
