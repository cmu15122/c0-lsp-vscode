import * as vscode from 'vscode';

export class ObjectFileEditorProvider implements vscode.CustomReadonlyEditorProvider {
    public static register(context: vscode.ExtensionContext): vscode.Disposable {
        const provider = new ObjectFileEditorProvider(context);
        return vscode.window.registerCustomEditorProvider(ObjectFileEditorProvider.viewType, provider);
    }

    private static readonly viewType = 'c0.objectFile';

    constructor(
        private readonly context: vscode.ExtensionContext
    ) {}

    /**
     * When a document is opened, this is called to allow us to prepare our
     * custom document model. Then {@linkcode resolveCustomEditor} is called
     * with this custom model.
     */
    openCustomDocument(uri: vscode.Uri, _openContext: vscode.CustomDocumentOpenContext, _token: vscode.CancellationToken): vscode.CustomDocument | Thenable<vscode.CustomDocument> {
        return new ObjectFile(uri);
    }

    /**
     * Invoked subsequent to {@linkcode openCustomDocument} when opening a
     * document. This controls the webview which is ultimately displayed.
     */
    resolveCustomEditor(document: vscode.CustomDocument, webviewPanel: vscode.WebviewPanel, token: vscode.CancellationToken): void | Thenable<void> {
        const displayUri= (document.uri.scheme === 'file') ? document.uri.fsPath : '&lt;filename&gt;';

        webviewPanel.webview.html = `
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>C0 Object File</title> <!-- TODO(cooper): should be document specific? -->
        </head>
        <body style="width: 100%;">
            <main style="text-align: center; margin: auto; width: 90%; position: absolute; top: 50%; bottom: 50%;">
               This document cannot be opened in VSCode. To see the interface
               for this object file, run <pre>cc0 -i ${displayUri}</pre>
               in your terminal.
            </main>
        </body>
        </html>`;
    }
}

class ObjectFile implements vscode.CustomDocument {
    public readonly uri: vscode.Uri;

    constructor(uri: vscode.Uri) {
        this.uri = uri;
    }

    public dispose(): void { return; }
}
