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

# Install

```
ln -s /Users/leonardo/Development/vult/src/lsp/vscode-extension ~/.vscode/extensions/vult-language-server
```