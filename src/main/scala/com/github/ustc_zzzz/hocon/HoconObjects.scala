package com.github.ustc_zzzz.hocon

/**
  * @author ustc_zzzz
  */
object HoconObjects {

  case class Sep(value: String) extends Element

  case class Open(value: String) extends Element

  case class Close(value: String) extends Element

  case class Number(value: String) extends Element

  case class Boolean(value: String) extends Element

  case class TimeUnit(value: String) extends Element

  case class NullPointer(value: String) extends Element

  case class QuotedString(value: String) extends Element

  case class UnquotedString(value: String) extends Element

  case class MultilineString(value: String) extends Element

  case class InclusionBodyPart(value: String) extends Element

  case class PathExpressionPart(value: String) extends Element

  case class CommentSpacesString(value: String) extends Element

  case class Spaces(start: Int, value: String, end: Int) extends Element

  case class PathExpression(path: Seq[Element]) extends ElementCollection("PathExpression") {
    override val values: IndexedSeq[Element] = path.flatMap(Sep(".") :: _ :: Nil).tail.toIndexedSeq

    override lazy val value: String = path.map(_.value).mkString(".")
  }

  case class SubstitutionWeak(path: PathExpression) extends ElementCollection("SubstitutionWeak") {
    override val values: IndexedSeq[Element] = Open("${?") +: path.values :+ Close("}")

    override lazy val value: String = "${?" + path.value + "}"
  }

  case class SubstitutionStrong(path: PathExpression) extends ElementCollection("SubstitutionStrong") {
    override val values: IndexedSeq[Element] = Open("${") +: path.values :+ Close("}")

    override lazy val value: String = "${" + path.value + "}"
  }

  case class Comment(prefix: String, spaces: Option[Spaces], comment: String) extends ElementCollection("Comment") {
    override def values: IndexedSeq[Element] = Open(prefix) +: spaces.toIndexedSeq :+ CommentSpacesString(comment)
  }

  case class Root(start: SpacesMultiline, element: Element, end: SpacesMultiline) extends ElementCollection("Root") {
    override val values: IndexedSeq[Element] = IndexedSeq(start, element, end)
  }

  case class SpacesMultiline(lines: Seq[(Option[Spaces], Int, Option[Comment])]) extends ElementCollection("SpacesMultiline") {
    override def values: IndexedSeq[Element] = lines.flatMap(l => Sep("\n") +: l._1.toSeq ++: l._3.toSeq).tail.toIndexedSeq
  }

  case class SepWithSpaces(prefix: SpacesMultiline, suffix: Option[(Sep, SpacesMultiline)]) extends ElementCollection("SepWithSpaces") {
    override def values: IndexedSeq[Element] = suffix match {
      case Some((a, b)) => IndexedSeq(prefix, a, b)
      case None => IndexedSeq(prefix)
    }
  }

  case class Concat(elements: Seq[Element], noString: scala.Boolean) extends ElementCollection(if (noString) "ConcatWithoutString" else "Concat") {
    override val values: IndexedSeq[Element] = elements.toIndexedSeq
  }

  case class List(start: SpacesMultiline, elements: Option[(ListElementPart, Seq[(SepWithSpaces, ListElementPart)], SepWithSpaces)]) extends ElementCollection("List") {
    override lazy val value: String = "[" + values.map(_.value).mkString + "]"

    override val values: IndexedSeq[Element] = start +: elements.toIndexedSeq.flatMap(t => t._1 +: t._2.flatMap(t => Seq(t._1, t._2)) :+ t._3)
  }

  case class Object(start: SpacesMultiline, elements: Option[(ObjectElementPart, Seq[(SepWithSpaces, ObjectElementPart)], SepWithSpaces)]) extends ElementCollection("Object") {
    override lazy val value: String = "{" + values.map(_.value).mkString + "}"

    override val values: IndexedSeq[Element] = start +: elements.toIndexedSeq.flatMap(t => t._1 +: t._2.flatMap(t => Seq(t._1, t._2)) :+ t._3)
  }

  case class RootObject(start: SpacesMultiline, elements: Option[(ObjectElementPart, Seq[(SepWithSpaces, ObjectElementPart)], SepWithSpaces)]) extends ElementCollection("RootObject") {
    override lazy val value: String = values.map(_.value).mkString

    override val values: IndexedSeq[Element] = start +: elements.toIndexedSeq.flatMap(t => t._1 +: t._2.flatMap(t => Seq(t._1, t._2)) :+ t._3)
  }

  case class ObjectElement(fieldPath: PathExpression, fieldValue: (SepWithSpaces, Concat)) extends ObjectElementPart {
    override def toString: String = values.mkString("ObjectElement(", ", ", ")")

    override def values: IndexedSeq[Element] = IndexedSeq(fieldPath, fieldValue._1, fieldValue._2)
  }

  case class Inclusion(prefix: Spaces, body: InclusionBodyPart) extends ObjectElementPart {
    override def values: IndexedSeq[Element] = IndexedSeq(Open("include"), prefix, body)
  }

  case class ListElement(element: Concat) extends ListElementPart {
    override def values: IndexedSeq[Element] = IndexedSeq(element)
  }

  sealed trait Element {
    def size: Int = value.length

    def value: String
  }

  sealed abstract class ElementCollection(name: String) extends Element {
    def values: IndexedSeq[Element]

    override lazy val value: String = values.map(_.value).mkString

    override def toString: String = values.mkString(name + "(", ", ", ")")
  }

  sealed abstract class ListElementPart extends ElementCollection("ListElementPart") {
    override def toString: String = values.head.toString
  }

  sealed abstract class ObjectElementPart extends ElementCollection("ObjectElementPart") {
    override def toString: String = values.mkString(", ")
  }
}
