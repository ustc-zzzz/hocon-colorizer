package com.github.ustc_zzzz.hocon

import io.scalajs.nodejs._

import scala.scalajs.js._

/**
  * @author ustc_zzzz
  */
object HoconColorizer {

  import com.github.ustc_zzzz.hocon.vscode.facade.VSCodeLanguageServer._
  import fastparse.core.Parsed

  def validateTextDocument(connection: Dynamic, textDocument: Dynamic): Unit = {
    val document = textDocument.getText().toString
    val diagnostics = HoconParsers.root.parse(document) match {
      case Parsed.Success(_, _) => Array()
      case Parsed.Failure(_, index, extra) =>
        val (startIndex, errorMessage) = HoconParsers.printError(extra.traced.stack.toIndexedSeq)
        val startPosition = textDocument.positionAt(startIndex)
        val endPosition = textDocument.positionAt(index)
        Array(Dictionary(
          // Error
          "severity" -> 1,
          "source" -> "hocon",
          "message" -> errorMessage,
          "range" -> Dictionary("start" -> startPosition, "end" -> endPosition)
        ))
    }
    connection.sendDiagnostics(Dictionary("uri" -> textDocument.uri, "diagnostics" -> diagnostics))
  }

  def main(): Unit = {
    val documents = new TextDocuments()
    val capabilities = Dictionary("textDocumentSync" -> documents.syncKind)
    val connection = createConnection(new IPCMessageReader(process), new IPCMessageWriter(process))

    documents.listen(connection)
    connection.onInitialize((_: Dynamic) => Dictionary("capabilities" -> capabilities))
    documents.onDidChangeContent((change: Dynamic) => validateTextDocument(connection, change.document))

    connection.listen()
  }
}