'use strict'

const path = require('path')
const vscode = require('vscode-languageclient')

/**
  * @author ustc_zzzz
  */
 exports.activate = function activate (context) {
  const runFileName = 'hoconcolorizer-opt.js'
  const debugFileName = 'hoconcolorizer-fastopt.js'

  const runAbsolutePath = path.join('target', 'scala-2.11', runFileName)
  const debugAbsolutePath = path.join('target', 'scala-2.11', debugFileName)

  const serverOptions = {
    run: {
      transport: vscode.TransportKind.ipc,
      module: context.asAbsolutePath(runAbsolutePath)
    },
    debug: {
      transport: vscode.TransportKind.ipc,
      module: context.asAbsolutePath(debugAbsolutePath),
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
