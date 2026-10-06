use zed_extension_api::{self as zed, Command, LanguageServerId, Result, Worktree};

struct VultExtension;

impl zed::Extension for VultExtension {
    fn new() -> Self {
        VultExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<Command> {
        // Prefer a `vult` binary on the worktree's PATH; fall back to the
        // usual install location if it isn't on PATH.
        let command = worktree
            .which("vult")
            .unwrap_or_else(|| "/usr/local/bin/vult".to_string());

        Ok(Command {
            command,
            args: vec!["-lsp".to_string()],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(VultExtension);
