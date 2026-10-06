# Vult Language Server Protocol (LSP) Implementation

This directory contains a complete Language Server Protocol implementation for the Vult DSP language.

## Features

###  Implemented Features

- **Diagnostics**: Real-time error reporting with Vult compiler integration
- **Semantic Highlighting**: Accurate syntax highlighting using the Vult lexer (replaces TextMate grammars)
  - Keywords, numbers, strings, comments, operators, punctuation
  - Function calls highlighted distinctly from variables
  - Uses actual Vult compiler tokenizer for perfect accuracy
- **Document Symbols**: "Go to Symbol" navigation showing:
  - Functions and external functions
  - Type definitions and structs
  - Enumerations and individual enum members
  - Constants
- **Go to Definition**: Navigate to function definitions from call sites
- **Basic Completions**: Keyword completion support
- **Workspace Support**: Multi-file project awareness

## Configuration

### Project Configuration File

Create a `vultconfig.json` file in your project root to configure include paths for module resolution:

```json
{
  "include": [
    "../util",
    "../common"
  ]
}
```

**Fields:**
- `include`: Array of paths where the LSP server will search for modules. Paths can be:
  - Relative (resolved relative to the config file location)
  - Absolute

The LSP server searches for `vultconfig.json` starting from the current file's directory and walking up to parent directories until found.

### Example

For a project structure like:
```
examples/
├── vultconfig.json
├── filters/
│   └── ladder.vult    (uses Util.cvTokHz)
└── util/
    └── util.vult      (defines Util module)
```

The `vultconfig.json` would contain:
```json
{
  "include": ["util"]
}
```

This allows `examples/filters/ladder.vult` to find the `Util` module from `examples/util/util.vult`.

## Running the server

The language server is part of the compiler; there is no separate server binary:

```
vult -lsp
```

Both the VS Code and the Zed extensions locate `vult` in `PATH` and launch it with `-lsp`. In VS Code the path can be
overridden with the `vult.languageServer.path` setting.

## Install

Build and install the compiler, then link the VS Code extension:

```
make compiler
sudo make install
make vscode-extension
ln -s "$PWD/src/lsp/vscode-extension" ~/.vscode/extensions/vult-language-server
```

`make vscode-extension` compiles `src/extension.ts` into `out/extension.js`, which is what VS Code actually loads. Run
it after changing the extension sources, otherwise VS Code keeps using the previously compiled client.