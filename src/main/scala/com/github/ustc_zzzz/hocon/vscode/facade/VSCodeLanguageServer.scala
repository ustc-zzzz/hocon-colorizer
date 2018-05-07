package com.github.ustc_zzzz.hocon.vscode.facade

import scala.scalajs.js._

/**
  * @author ustc_zzzz
  */
@native
@annotation.JSImport("vscode-languageserver", annotation.JSImport.Namespace)
object VSCodeLanguageServer extends Object {

  @native
  class IPCMessageReader(process: Any) extends Object

  @native
  class IPCMessageWriter(process: Any) extends Object

  @native
  class TextDocuments() extends Object {
    def syncKind: Dynamic = native

    def onDidChangeContent: Dynamic = native

    def onDidOpen: Dynamic = native

    def onWillSave: Dynamic = native

    def onWillSaveWaitUntil(handler: Any): Dynamic = native

    def onDidSave: Dynamic = native

    def onDidClose: Dynamic = native

    def get(uri: Any): Dynamic = native

    def all: Dynamic = native

    def keys: Dynamic = native

    def listen(connection: Any): Dynamic = native
  }

  def createConnection(inputStream: Any, outputStream: Any): Dynamic = native
}
