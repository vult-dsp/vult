import * as vscode from 'vscode';
import * as fs from 'fs';
import { spawn } from 'child_process';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient;

// Locate the `vult` compiler in PATH; it doubles as the language server via `vult -lsp`
async function findVultInPath(): Promise<string | null> {
  return new Promise((resolve) => {
    const process = spawn('which', ['vult'], { shell: true });
    let output = '';

    process.stdout.on('data', (data) => {
      output += data.toString();
    });

    process.on('close', (code) => {
      const found = output.trim();
      if (code === 0 && found && fs.existsSync(found)) {
        resolve(found);
        return;
      }
      resolve(null);
    });

    process.on('error', () => {
      resolve(null);
    });
  });
}

// Function to find the Vult compiler that will act as the language server
async function findLanguageServer(): Promise<string> {
  const config = vscode.workspace.getConfiguration('vult');
  const configuredPath = config.get<string>('languageServer.path');

  // If user has explicitly configured a path, use it
  if (configuredPath) {
    if (!fs.existsSync(configuredPath)) {
      throw new Error(`User-configured Vult compiler not found at: ${configuredPath}`);
    }
    console.log(`Using user-configured Vult compiler at: ${configuredPath}`);
    return configuredPath;
  }

  const pathToVult = await findVultInPath();
  if (pathToVult) {
    console.log(`Found Vult compiler in PATH: ${pathToVult}`);
    return pathToVult;
  }

  throw new Error(
    'No Vult compiler found. Install `vult` in your PATH or set "vult.languageServer.path".'
  );
}

export function activate(context: vscode.ExtensionContext) {
  console.log('Vult Language Server extension is now active');

  findLanguageServer().then(serverPath => {
    console.log(`Starting Vult Language Server: ${serverPath} -lsp`);

    // No `transport` field: for an Executable the client appends `--stdio` when
    // TransportKind.stdio is set, and `vult` rejects unknown options. Leaving it
    // undefined selects the very same stdio pipes without the extra argument.
    const serverExecutable = {
      command: serverPath,
      args: ['-lsp'],
      options: {
        shell: false,
        env: { ...process.env }
      }
    };

    const serverOptions: ServerOptions = { run: serverExecutable, debug: serverExecutable };

  // Client options - configure how the client communicates with the server
  const clientOptions: LanguageClientOptions = {
    // Register the server for Vult files
    documentSelector: [
      { scheme: 'file', language: 'vult' },
      { scheme: 'untitled', language: 'vult' }
    ],

    // Synchronize file events
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.vult')
    },

    // Output channel for debugging
    outputChannelName: 'Vult Language Server',

    // Trace level from configuration
    traceOutputChannel: vscode.window.createOutputChannel('Vult Language Server Trace')
  };

  // Create and start the language client
  client = new LanguageClient(
    'vultLanguageServer',
    'Vult Language Server',
    serverOptions,
    clientOptions
  );

  // Register commands
  const restartCommand = vscode.commands.registerCommand('vult.restartLanguageServer', () => {
    client.restart();
    vscode.window.showInformationMessage('Vult Language Server restarted');
  });

  const showOutputCommand = vscode.commands.registerCommand('vult.showServerOutput', () => {
    client.outputChannel.show();
  });

  context.subscriptions.push(restartCommand, showOutputCommand);

    // Start the client (this will also launch the server)
    client.start().then(() => {
      console.log('Vult Language Server started successfully');
      vscode.window.showInformationMessage('Vult Language Server is ready!');
    }).catch((error) => {
      console.error('Failed to start Vult Language Server:', error);
      vscode.window.showErrorMessage(`Failed to start Vult Language Server: ${error.message}`);
    });
  }).catch((error) => {
    console.error('Failed to find Vult Language Server:', error);
    vscode.window.showErrorMessage(`Failed to find Vult Language Server: ${error.message}`);
  });
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  console.log('Deactivating Vult Language Server');
  return client.stop();
}
