# Vult Compiler Structure

This document describes the architecture of the Vult compiler as of February 2026.

## Overview

The Vult compiler transforms Vult DSP source code into various target languages (C++, Lua, JavaScript, Julia, Python, Java). The compilation process is organized into distinct phases with clear boundaries between them.

## Directory Structure

```
src/
├── pparser/          # Parsing (lexer, parser, syntax AST)
├── core/             # Type checking, elaboration, IR transformations
├── generators/       # Code generation for target languages
├── driver/           # CLI driver
├── lsp/              # Language Server Protocol implementation
├── util/             # Utility functions and common types
├── node/             # Node.js bindings
├── formatter/        # Code formatter
├── vult.ml           # Main executable entry point
└── vultjs.ml         # JavaScript/Node.js entry point
```

## Compilation Pipeline

```
┌─────────────────────────────────────────────────────────────────┐
│                         FRONT END                                │
├─────────────────────────────────────────────────────────────────┤
│  1. PARSING                    → Syntax.program                  │
│     (src/pparser/)               (untyped AST)                   │
│                                                                  │
│  2. TYPE CHECKING              → Typed.program + Env             │
│     (src/core/typechecking.ml)   (typed AST with EGenCall nodes) │
│     - Type inference via unification                             │
│     - Error reporting                                            │
│     - LSP diagnostics stop here                                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                        MIDDLE END                                │
├─────────────────────────────────────────────────────────────────┤
│  3. ELABORATION                → Typed.program                   │
│     (src/core/elaboration.ml)    (no EGenCall, fully resolved)   │
│     - Generic function instantiation                             │
│     - Type intrinsic resolution (typedefault, typemax, typemin)  │
│     - Constant parameter substitution                            │
│                                                                  │
│  4. LOWERING                   → Prog.program                    │
│     (src/core/toprog.ml)         (code generation IR)            │
│     - Type conversion Typed → Prog                               │
│     - Initializer/serializer generation                          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                         BACK END                                 │
├─────────────────────────────────────────────────────────────────┤
│  5. OPTIMIZATION               → Prog.program (optimized)        │
│     (src/core/passes.ml)                                         │
│     - Constant folding                                           │
│     - Dead code elimination                                      │
│     - Inlining                                                   │
│                                                                  │
│  6. CODE GENERATION            → Target code                     │
│     (src/generators/cpp.ml, js.ml, lua.ml, etc.)                 │
│     - C++, JavaScript, Lua, Julia, Python, Java                  │
└─────────────────────────────────────────────────────────────────┘
```

## Core Modules

### Parsing (`src/pparser/`)

| File | Purpose |
|------|---------|
| `lexer.mll` | OCamllex lexer specification |
| `tokens.ml` | Token definitions |
| `parse.ml` | Parser implementation |
| `syntax.ml` | Untyped AST definitions |
| `stream.ml` | Token stream utilities |
| `ptags.ml` | Tag/attribute handling |

**Key Type:** `Syntax.program` - the untyped abstract syntax tree

### Type Checking (`src/core/typechecking.ml`)

Performs type inference using Hindley-Milner style unification. Key responsibilities:
- Type unification (`unify`, `unifyRaise`)
- Expression typing (`exp`, `exp_list`)
- Statement typing (`stmt`, `stmt_list`)
- Function definition processing
- Generic function registration (stores `EGenCall` nodes)

**Key Functions:**
- `typecheck`: Type checking only (returns `EGenCall` nodes unprocessed)
- `typecheck_and_elaborate`: Full pipeline (type check + elaboration)
- `typecheck_single`: Type check a single file (used by interpreter)

**Key Types:**
- `Typed.exp`, `Typed.stmt`, `Typed.function_def` - typed AST
- `Env.env` - type environment

### Elaboration (`src/core/elaboration.ml`)

Transforms the typed AST by instantiating generic functions. Key responsibilities:
- Generic function instantiation
- `EGenCall` → `ECall` transformation
- Type intrinsic resolution (`typedefault()`, `typemax()`, `typemin()`)
- Constant parameter substitution

**Key Function:**
- `elaborate`: Takes typed AST with `EGenCall` nodes, returns fully elaborated AST

### Environment (`src/core/env.ml`)

Manages the type environment during compilation:
- Module management
- Type definitions
- Function signatures
- Variable scopes
- Generic function registry

### IR Lowering (`src/core/toprog.ml`)

Converts the typed AST to the `Prog` intermediate representation:
- Type conversion from `Typed` to `Prog`
- Initializer generation
- Serializer generation

**Key Type:** `Prog.program` - the code generation IR

### Passes (`src/core/passes.ml`)

Performs transformation passes on the `Prog` IR to prepare it for code generation. The passes are applied iteratively until no more changes occur:

| Pass | Purpose |
|------|---------|
| `Location` | Tracks context (current function, current type, if-expression scope) |
| `Markers` | Marks function scope for subsequent passes |
| `Canonize` | Normalizes expressions (reorders commutative operations, converts `a - b` to `a + (-b)`) |
| `StrengthReduction` | Replaces expensive operations with cheaper equivalents (e.g., `x * 2^n` → `x << n`, `pow(x,2)` → `x * x`) |
| `Simplify` | Evaluates constant expressions, removes identity operations (`a = a`), eliminates empty blocks |
| `Builtin` | Evaluates built-in functions with constant arguments (`pi`, `exp`, `sin`, `cos`, `abs`, `sqrt`, `samplerate`, `size`, `length`) |
| `IfExpressions` | Transforms if-expressions into statements with temporary variables |
| `Tuples` | Handles multi-return function calls by binding results to context members |
| `Cast` | Processes type cast functions (`fix16`, `real`, `int`, `bool`) and handles `real` → `fix16` mode |
| `LiteralArrays` | Binds array literals to temporary variables |
| `LiteralRecords` | Binds record literals to temporary variables |
| `Sort` | Topologically sorts types and functions by dependencies |

### Initializer Generation (`src/core/initializer.ml`)

Generates initialization functions for struct types:
- Creates `_init` (for C++/reference-style) or `_alloc` (for value-style) functions
- Handles default values for all basic types
- Processes `@init` tags to set custom initial values
- Supports type intrinsics (`typemax`, `typemin`, `typedefault`) in init tags
- Generates initialization loops for arrays
- Calls custom initializer functions if defined

### Serializer Generation (`src/core/serializer.ml`)

Generates serialization/deserialization functions for types with `@save` tags:
- Creates `_serialize_data` functions that write struct members to a buffer
- Creates `_deserialize_data` functions that read members from a buffer
- Generates type descriptors for the serialization format
- Handles arrays and lists of both primitive and struct types
- Propagates `@save` tags through type dependencies

### Code Generation (`src/generators/`)

| File | Target Language |
|------|----------------|
| `cpp.ml` | C++ |
| `js.ml` | JavaScript |
| `lua.ml` | Lua |
| `julia.ml` | Julia |
| `python.ml` | Python |
| `java.ml` | Java |
| `tables.ml` | Lookup table generation |
| `tocode.ml` | Common code generation utilities |

## Type System

### Basic Types
- `int` - 32-bit integer
- `int16` - 16-bit integer (for DSP)
- `real` - floating point (double)
- `fix16` - 16.16 fixed point
- `bool` - boolean
- `string` - string literals

### Composite Types
- `array(T, N)` - fixed-size arrays
- `tuple` - heterogeneous tuples
- Records (user-defined types)
- Enumerations

### Generic Functions

Vult supports generic functions with:
- Type parameters (inferred from arguments)
- Constant parameters (compile-time values)
- Function parameters (higher-order generics)

Generic parameters are prefixed with `'` (single quote). Examples:

```vult
// Type parameter - 't is inferred from the argument type
fun change(x : 't) : bool {
   mem pre : 't;
   val result = pre <> x;
   pre = x;
   return result;
}

// Function parameter - 'f is a function passed as argument
fun apply_func('f, a, b) {
   return f(a, b);
}

// Constant parameter with explicit type annotation
fun add_const('n : int, x : int) : int {
   return n + x;
}
```

Usage:
```vult
val x = change(1.0);              // Specialized for real
val y = change(1);                // Specialized for int
val z = apply_func(add_int, 3, 4); // Pass function as parameter
val w = add_const(10, 5);         // Constant 10 inlined at compile time
```

## LSP Integration

The LSP (`src/lsp/`) uses `Typechecking.typecheck` (without elaboration) for diagnostics since type errors are sufficient for editor feedback. This is more efficient than running full elaboration.

## Interpreter

The interpreter (`src/core/interpreter.ml`) provides:
- Runtime evaluation of Vult expressions
- Audio rendering capabilities
- REPL support

## Building and Testing

```bash
# Build
dune build

# Run tests
make test-fast

# Build JavaScript version
dune build src/vult.bc
```

## Historical Note

This architecture was reorganized in February 2026 to separate type checking from elaboration. Previously, both phases were combined in a single `inference.ml` file (~3000 lines). The separation allows:
1. More efficient LSP diagnostics
2. Clearer code organization
3. Better separation of concerns
