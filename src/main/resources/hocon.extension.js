'use strict'

const path = require('path')
const vscode = require('vscode-languageclient')

exports.activate = function activate (context) {
  const fileName = 'hoconcolorizer-opt.js'

  const absolutePath = path.join('target', 'scala-2.11', fileName)
  const serverModuleAbsolutePath = context.asAbsolutePath(absolutePath)

  const serverOptions = {
    run: {
      module: serverModuleAbsolutePath,
      transport: vscode.TransportKind.ipc
    },
    debug: {
      module: serverModuleAbsolutePath,
      transport: vscode.TransportKind.ipc,
      options: { execArgv: ['--nolazy', '--debug=6009'] }
    }
  }
  const clientOptions = {
    documentSelector: [{scheme: 'file', language: 'hocon'}]
  }

  const name = 'HOCON Language Server'
  const client = new vscode.LanguageClient(name, serverOptions, clientOptions)

  context.subscriptions.push(client.start())
}
