# Plan: calling function parameters of generic functions

Status: not implemented. Written 2026-10-15 on branch `v1`.

## The problem

A generic function can declare a function parameter, but calling it fails:

```vult
fun apply('f, x : int, y : int) : int {
  return f(x, y);
}
fun add(a : int, b : int) : int { return a + b; }
fun test() : int { return apply(add, 1, 2); }
```

```
Error: ...:2:10-11: A function with the name 'f' could not be found
```

This has never worked. `test/interpreter/generics_function_params.vult` was added in `bff13343` with a
note saying it is not part of the test suite, and that revision fails with the same error.

## What happens today

1. Parsing (`Parse`, `generic_param`): `'f` and `'f : (int, int) -> int` become `GParamFunction`.
   An untyped parameter is a function only if its name starts with `f`; otherwise it is a constant.
2. Definition (`Typechecking.create_generic_function`): the generic is stored with its parameters.
   Its body is not typechecked here.
3. Call site (`Typechecking.generic_call`): the explicit arguments are checked with
   `generic_arg_context`, so `add` becomes `EId "add"` with type `TEFunction` (the `ExprFunction`
   case of `exp`). The call is an `EGenCall` whose params include
   `CallGeneric {param= GParamFunction _; value= EId "add"}`. This part works.
4. Elaboration (`Elaboration.find_or_instantiate`):
   - `callExplicitArgs` includes the function reference, and `is_constant_literal` is false for an
     `EId`, so `all_constants` is false and the call takes the non-specialized ("dyn") path.
   - `build_structural_signature` keys the explicit arguments by type only. `apply(add, ..)` and
     `apply(sub, ..)` produce the same key.
5. Instantiation (`Elaboration.instantiate_generic_function`): `specialize_param` turns the function
   parameter into `ResolvedParam` and drops it. The body is typechecked with
   `stmt_with_type_substitution`, and `f(x, y)` reaches `Typechecking.call`, which finds no function,
   generic or companion named `f` and raises the error.

## Design

Function parameters are always resolved at compile time. The targets have no function values, so a
function argument can never become a runtime argument, even when the constant parameters of the same
call are not literals ("dyn" mode).

### 1. Bind the function parameters in the environment

Add to `Env.env` (next to `loop_counters`) a map from the parameter name to the function it stands
for:

```ocaml
; function_params: path Map.t (* The function bound to each function parameter of the specialization being instantiated *)
```

- `instantiate_generic_function` sets the bindings from the `CallGeneric {param= GParamFunction (name, _); value= {e= EId ..}}`
  params before typechecking the body and restores the previous map afterwards (specializations
  nest).
- `Typechecking.call`: before the regular lookup, if `path.n = None` and `path.id` is bound, continue
  with the bound path. Everything after that is the regular call path: type application,
  `propagateVariability` and `addContextArg` (instances for functions with `mem`).
- `lookupExpressionSymbol` in `generic_arg_context`: a bound name resolves to the bound function, so a
  generic can pass its function parameter on to another generic (`apply2('g, ..) { return apply(g, ..); }`).

Binding in the environment, instead of renaming the calls in the syntax tree, also covers the
companion functions (`and`) of the generic, which are instantiated with `function_def_opt`. Check that
the bindings are still in place when the companions are processed.

The value of the parameter must carry the full path of the function (module included). The `EId` built
in the `ExprFunction` case only keeps the name, so either store `f.path` there (an `EFunRef path`
expression, or the path in the `EId`) or resolve the name again at instantiation from the module of
the call site.

### 2. Key the specializations by function

- `build_structural_signature`: for a `GParamFunction` param, add the module and name of the passed
  function to the key.
- `build_signature_string` and its variants: add the function name to the emitted name, e.g.
  `apply_add_int_int`.
- The function parameters must not count as explicit arguments when deciding `all_constants`
  (`callExplicitArgs`, `is_constant_literal`). Only `GParamConstant` values decide between the
  specialized and the "dyn" path.
- `processed_args` already drops function params, so nothing is passed at runtime.

### 3. Check the passed function

At the call site (`generic_call`, where constant params are checked against their declared types):

- the value must be a function reference, not any expression of function type;
- when the parameter is annotated (`'f : (int, int) -> int`), unify a fresh copy of the annotation with
  the function's type;
- report generic functions passed as arguments with a clear error, unless they are supported (see open
  questions).

### 4. Functions with memory

Calling a passed function that has `mem` variables needs an instance in the context of the
specialization. `addContextArg` creates one for a regular call. Things to check:

- each specialization gets its own instance, since `apply(counter, ..)` and `apply(other, ..)` are
  different specializations;
- explicit instances in the body (`inst:f(x)`) resolve through the binding as well;
- the specialization is marked as active (has a context) when the passed function is.

## Tests

1. Move `test/interpreter/generics_function_params.vult` into the interpreter tests and drop its
   "NOT in the test suite" note.
2. Cases:
   - typed and untyped function parameters;
   - the same generic called with two different functions of the same type (`add` and `sub`), both
     results correct (the specialization key);
   - a function from another module (`Util.clip`);
   - a function with `mem` passed twice, each specialization keeping its own state;
   - a function parameter passed on to another generic;
   - a generic with a function parameter and a non-literal constant parameter ("dyn" constants with a
     resolved function);
   - a companion function of the generic calling the function parameter;
   - errors: a non-function argument, a function whose type does not match the annotation, calling a
     name that is not bound.
3. Code generation for C++ and JS on one of the cases, to check the emitted names and instances.

## Open questions

- Should a generic function be allowed as the argument (`apply(scale_typed, ..)`)? It would need to be
  instantiated with the argument types of the call inside the body.
- Should builtins (`abs`, `clip`) be allowed as arguments? They have no `Env.f` in a module, so the
  binding would need to hold a builtin as well.
- The naming convention for untyped parameters (`'f..` is a function, anything else a constant) is
  surprising. Consider requiring the annotation or inferring the kind from the use in the body.
