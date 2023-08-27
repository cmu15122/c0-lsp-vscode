import * as path from 'path';
import { workspace, ExtensionContext, extensions } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';
import { ObjectFileEditorProvider } from './objectFileEditor';
import { SourceFileBlocker, SourceFileAllower } from './sourceFileEditors';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    // Register custom "editor" for object files. This is really a read-only
    // view into the file, where we can display arbitrary content, but the
    // VSCode API formulates this as an editor.
    context.subscriptions.push(ObjectFileEditorProvider.register(context));

    if (hasBlacklistedExtension()) {
        // Register custom "editor" for source files. This just wraps the check for
        // copilot et al. so we can disable editing features if those are installed.
        context.subscriptions.push(SourceFileBlocker.register(context));
    } else {
        context.subscriptions.push(SourceFileAllower.register(context));
    }

    // The server is implemented in node
    const serverModule = context.asAbsolutePath(
        path.join('server', 'out', 'server.js')
    );
    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
    const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions: ServerOptions = {
        run: { module: serverModule, transport: TransportKind.ipc },
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: debugOptions
        }
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for c0 documents
        documentSelector: [{ scheme: 'file', language: 'C0' }],
        synchronize: {
            // Notify the server about file changes to .c0 or .c1 files, for dependencies
            fileEvents: [workspace.createFileSystemWatcher('**/*.c0'),
            workspace.createFileSystemWatcher('**/*.c1'),
            workspace.createFileSystemWatcher('**/project.txt')]
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'c0LanguageServer',
        'C0 Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

// List of extension IDs for disabled extensions. 
const extensionBlacklist = [
    "github.copilot"
];

function hasBlacklistedExtension(): boolean {
    for (const id of extensionBlacklist) {
        if (extensions.getExtension(id) !== undefined) {
            return true;
        }
    }

    return false;
}
