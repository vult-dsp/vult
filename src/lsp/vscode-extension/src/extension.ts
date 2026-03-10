import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { spawn } from 'child_process';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

// Function to check if native LSP server is available in PATH
async function checkNativeLspInPath(): Promise<string | null> {
  return new Promise((resolve) => {
    const process = spawn('which', ['vult-lsp'], { shell: true });
    let output = '';
    
    process.stdout.on('data', (data) => {
      output += data.toString();
    });
    
    process.on('close', (code) => {
      if (code === 0 && output.trim()) {
        const path = output.trim();
        // Verify the file exists and is executable
        if (fs.existsSync(path)) {
          resolve(path);
          return;
        }
      }
      resolve(null);
    });
    
    process.on('error', () => {
      resolve(null);
    });
  });
}

// Function to find the LSP server
async function findLspServer(context: vscode.ExtensionContext): Promise<{path: string, isNative: boolean}> {
  const config = vscode.workspace.getConfiguration('vult');
  let serverPath = config.get<string>('languageServer.path');
  
  // If user has explicitly configured a path, use it
  if (serverPath) {
    if (fs.existsSync(serverPath)) {
      console.log(`Using user-configured Vult Language Server at: ${serverPath}`);
      return { path: serverPath, isNative: true };
    } else {
      throw new Error(`User-configured Vult Language Server not found at: ${serverPath}`);
    }
  }
  
  // Try to find native version in PATH
  const nativePath = await checkNativeLspInPath();
  if (nativePath) {
    console.log(`Found native Vult Language Server in PATH: ${nativePath}`);
    return { path: nativePath, isNative: true };
  }
  
  // Fallback to bundled JavaScript version
  const jsServerPath = path.join(context.extensionPath, 'vult-lsp.js');
  if (fs.existsSync(jsServerPath)) {
    console.log(`Using bundled JavaScript Vult Language Server: ${jsServerPath}`);
    return { path: jsServerPath, isNative: false };
  }
  
  throw new Error('No Vult Language Server found. Please install vult-lsp or build the project.');
}

export function activate(context: vscode.ExtensionContext) {
  console.log('Vult Language Server extension is now active');

  findLspServer(context).then(serverInfo => {
    const { path: serverPath, isNative } = serverInfo;
    
    console.log(`Using ${isNative ? 'native' : 'JavaScript'} Vult Language Server at: ${serverPath}`);

    // Server options - different for native vs JavaScript
    const serverOptions: ServerOptions = isNative ? {
      // Native executable
      run: { 
        command: serverPath, 
        transport: TransportKind.stdio,
        options: { 
          shell: false,
          env: { ...process.env }
        }
      },
      debug: { 
        command: serverPath, 
        transport: TransportKind.stdio,
        options: { 
          shell: false,
          env: { ...process.env }
        }
      }
    } : {
      // JavaScript version - run with node
      run: { 
        command: 'node', 
        args: [serverPath],
        transport: TransportKind.stdio,
        options: { 
          shell: false,
          env: { ...process.env }
        }
      },
      debug: { 
        command: 'node', 
        args: [serverPath],
        transport: TransportKind.stdio,
        options: { 
          shell: false,
          env: { ...process.env }
        }
      }
    };

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
      console.log(`Vult Language Server (${isNative ? 'native' : 'JavaScript'}) started successfully`);
      vscode.window.showInformationMessage(`Vult Language Server (${isNative ? 'native' : 'JavaScript'}) is ready!`);
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