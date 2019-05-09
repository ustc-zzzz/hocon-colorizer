package com.github.ustc_zzzz.hocon

import scala.scalajs.js._

/**
  * @author ustc_zzzz
  */
object HoconColorizer {

  import com.github.ustc_zzzz.hocon.HoconObjects._
  import com.github.ustc_zzzz.hocon.vscode.facade.VSCodeLanguageServer._

  private val documents: TextDocuments = new TextDocuments()
  private val abstractSyntaxTrees: collection.mutable.Map[Any, Root] = collection.mutable.Map()

  def formatTextDocument(connection: Dynamic, textDocument: Dynamic, options: Dynamic): Array[TextEdit] = {
    val builder = Array[TextEdit]()
    abstractSyntaxTrees.get(textDocument.uri) match {
      case None => ()
      case Some(root) =>
        val useTabs = (!options.insertSpaces).asInstanceOf[scala.Boolean]
        val posAt = (pos: Int) => documents.get(textDocument.uri).positionAt(pos)
        val tabSpace = if (useTabs) "\t" else " " * options.tabSize.asInstanceOf[Int]
        HoconFormatter.format(root, HoconFormatter.IndentOptions(builder, posAt, tabSpace))
    }
    builder
  }

  def validateTextDocument(connection: Dynamic, textDocument: Dynamic): Unit = {
    import fastparse._
    val uri = textDocument.uri
    val diagnostics = Array[Any]()
    parse(textDocument.getText().toString, HoconParsers.root(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => abstractSyntaxTrees.put(uri, value)
      case Parsed.Failure(_, index, extra) =>
        val (startIndex, errorMessage) = HoconParsers.printError(extra.stack.toIndexedSeq)
        val startPosition = textDocument.positionAt(startIndex)
        val endPosition = textDocument.positionAt(index)
        abstractSyntaxTrees.remove(uri)
        diagnostics.push(Dictionary(
          // Error
          "severity" -> 1,
          "source" -> "hocon",
          "message" -> errorMessage,
          "range" -> Dictionary("start" -> startPosition, "end" -> endPosition)
        ))
    }
    connection.sendDiagnostics(Dictionary("uri" -> uri, "diagnostics" -> diagnostics))
  }

  def main(): Unit = {
    import io.scalajs.nodejs._
    val connection = createConnection(new IPCMessageReader(process), new IPCMessageWriter(process))
    val capabilities = Dictionary("textDocumentSync" -> documents.syncKind, "documentFormattingProvider" -> true)
    connection.onDocumentFormatting((e: Dynamic) => formatTextDocument(connection, e.textDocument, e.options))
    documents.onDidChangeContent((e: Dynamic) => validateTextDocument(connection, e.document))
    connection.onInitialize((_: Dynamic) => Dictionary("capabilities" -> capabilities))
    documents.listen(connection)
    connection.listen()
  }
}
