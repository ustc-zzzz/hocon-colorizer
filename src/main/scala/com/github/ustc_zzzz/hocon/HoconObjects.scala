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

  case class Spaces(start: Int, spaces: String, end: Int) extends Element {
    override def value: String = spaces
  }

  case class Comment(prefix: String, spaceStart: Int, spaces: String, spaceEnd: Int, comment: String) extends Element {
    override def value: String = prefix + spaces + comment
  }

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

  case class Root(start: SpacesMultiLine, element: Element, end: SpacesMultiLine) extends ElementCollection("Root") {
    override val values: IndexedSeq[Element] = IndexedSeq(start, element, end)
  }

  case class SpacesSingleLine(spaces: Option[Spaces], index: Int, comment: Option[Comment]) extends ElementCollection("SpacesSingleLine") {
    override def values: IndexedSeq[Element] = spaces.toIndexedSeq ++ comment.toIndexedSeq
  }

  case class SpacesMultiLine(lines: Seq[SpacesSingleLine]) extends ElementCollection("SpacesMultiLine") {
    override def values: IndexedSeq[Element] = lines.flatMap(Seq(Sep("\n"), _)).toIndexedSeq
  }

  case class SepWithSpaces(prefix: SpacesMultiLine, suffix: Option[(Sep, SpacesMultiLine)]) extends ElementCollection("SepWithSpaces") {
    override def values: IndexedSeq[Element] = suffix match {
      case Some((a, b)) => IndexedSeq(prefix, a, b)
      case None => IndexedSeq(prefix)
    }
  }

  case class Concat(elements: Seq[Element], noString: scala.Boolean) extends ElementCollection(if (noString) "ConcatWithoutString" else "Concat") {
    override val values: IndexedSeq[Element] = elements.toIndexedSeq
  }

  case class List(start: SpacesMultiLine, elements: Seq[(ListElementPart, SepWithSpaces)]) extends ElementCollection("List") {
    override lazy val value: String = "[" + values.map(_.value).mkString + "]"

    override val values: IndexedSeq[Element] = (start +: elements.flatMap(t => t._1 :: t._2 :: Nil)).toIndexedSeq
  }

  case class Object(start: SpacesMultiLine, elements: Seq[(ObjectElementPart, SepWithSpaces)]) extends ElementCollection("Object") {
    override lazy val value: String = "{" + values.map(_.value).mkString + "}"

    override val values: IndexedSeq[Element] = (start +: elements.flatMap(t => t._1 :: t._2 :: Nil)).toIndexedSeq
  }

  case class RootObject(start: SpacesMultiLine, elements: Seq[(ObjectElementPart, SepWithSpaces)]) extends ElementCollection("RootObject") {
    override lazy val value: String = values.map(_.value).mkString

    override val values: IndexedSeq[Element] = (start +: elements.flatMap(t => t._1 :: t._2 :: Nil)).toIndexedSeq
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
