# Vult language support for Zed

A [Zed](https://zed.dev) extension that registers the Vult DSP language
(`.vult` files) and connects it to the Vult language server (`vult -lsp`),
providing diagnostics, hover, completion and semantic-token highlighting.

This is the Zed counterpart to the VS Code extension in
[`../vscode-extension`](../vscode-extension).

## Requirements

- A `vult` binary on your `PATH` (or at `/usr/local/bin/vult`).
- Zed builds dev extensions with **rustup** — a system/package-manager Rust
  install will not work. Install rustup and the WebAssembly targets:

  ```bash
  rustup default stable
  rustup target add wasm32-wasip1 wasm32-wasip2
  ```

  (On Arch Linux, `rustup` replaces the `rust` package:
  `sudo pacman -S --needed rustup`.)

## Install

1. In Zed, open the command palette and run **`zed: install dev extension`**
   (or Extensions → *Install Dev Extension*).
2. Select this directory (`src/lsp/zed-extension`).
3. Open any `.vult` file — the language server starts automatically.

## Files

- `extension.toml` — extension + language-server registration.
- `languages/vult/config.toml` — file association (`.vult`), comments, brackets.
- `src/lib.rs` — returns the `vult -lsp` command to launch.

## Syntax highlighting

This extension ships no Tree-sitter grammar — highlighting comes from the
language server's **LSP semantic tokens**. Zed keeps semantic tokens off by
default, so enable them for Vult in your `settings.json`:

```json
{
  "languages": {
    "Vult": { "semantic_tokens": "full" }
  }
}
```

Use `"full"` (semantic tokens are the sole source of highlighting) since there
is no grammar to combine with.
