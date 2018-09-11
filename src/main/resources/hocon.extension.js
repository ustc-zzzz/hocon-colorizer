'use strict'

const path = require('path')
const {TransportKind, LanguageClient} = require('vscode-languageclient')

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
      transport: TransportKind.ipc,
      module: context.asAbsolutePath(runAbsolutePath)
    },
    debug: {
      transport: TransportKind.ipc,
      module: context.asAbsolutePath(debugAbsolutePath)
    }
  }
  const clientOptions = {
    documentSelector: [{scheme: 'file', language: 'hocon'}]
  }

  const name = 'HOCON Language Server'
  const client = new LanguageClient(name, serverOptions, clientOptions)

  context.subscriptions.push(client.start())
}
