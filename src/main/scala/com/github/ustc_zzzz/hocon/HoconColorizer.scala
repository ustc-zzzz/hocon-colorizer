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
        val position = textDocument.positionAt(index)
        Array(Dictionary(
          "severity" -> 1, // Error
          "source" -> "HOCON Colorizer",
          "range" -> Dictionary("start" -> position, "end" -> position),
          "message" -> s"Expected ${extra.traced.traceParsers.mkString(" | ")}"
        ))
    }
    connection.sendDiagnostics(Dictionary("uri" -> textDocument.uri, "diagnostics" -> diagnostics))
  }

  def main(): Unit = {
    val connection = createConnection(new IPCMessageReader(process), new IPCMessageWriter(process))
    val documents = new TextDocuments()
    documents.listen(connection)

    connection.onInitialize { params: Dynamic =>
      Dictionary("capabilities" -> Dictionary(
        "textDocumentSync" -> documents.syncKind,
        "completionProvider" -> Dictionary("resolveProvider" -> true)
      ))
    }

    documents.onDidChangeContent { change: Dynamic =>
      validateTextDocument(connection, change.document)
    }

    connection.listen()
  }
}
