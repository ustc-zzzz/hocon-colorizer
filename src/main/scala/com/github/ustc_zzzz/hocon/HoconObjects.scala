package com.github.ustc_zzzz.hocon

/**
  * @author ustc_zzzz
  */
object HoconObjects {

  case class Space(value: String) extends Element

  case class Number(value: String) extends Element

  case class Boolean(value: String) extends Element

  case class TimeUnit(value: String) extends Element

  case class NullPointer(value: String) extends Element

  case class QuotedString(value: String) extends Element

  case class UnquotedString(value: String) extends Element

  case class MultilineString(value: String) extends Element

  case class PathExpressionPart(value: String) extends Element

  case class PathExpression(path: Seq[Element]) extends ElementCollection("PathExpression") {
    override lazy val value: String = path.map(_.value).mkString(".")

    override val values: IndexedSeq[Element] = path.toIndexedSeq

    override def offset(i: Int): Int = if (i > 0) 1 else 0
  }

  case class SubstitutionWeak(path: PathExpression) extends ElementCollection("SubstitutionWeak") {
    override def offset(index: Int): Int = if (index > 0) 1 else 3

    override lazy val value: String = "${?" + path.value + "}"

    override val values: IndexedSeq[Element] = path.values
  }

  case class SubstitutionStrong(path: PathExpression) extends ElementCollection("SubstitutionStrong") {
    override def offset(index: Int): Int = if (index > 0) 1 else 2

    override lazy val value: String = "${" + path.value + "}"

    override val values: IndexedSeq[Element] = path.values
  }

  case class List(start: Int, elements: IndexedSeq[ListElement], end: Int) extends ElementCollection("List") {
    override lazy val value: String = values.map(_.value).mkString("[", ", ", "]")

    override val values: IndexedSeq[Element] = elements.map(_.element)

    override def offset(index: Int): Int = elements(index).offset

    override def size: Int = end - start
  }

  case class Object(start: Int, elements: IndexedSeq[ObjectElement], end: Int) extends ElementCollection("Object") {
    override def toString: String = elements.map(t => s"${t.path} -> ${t.element._2}").mkString("Object(", ", ", ")")

    override lazy val value: String = elements.map(t => s"${t.path}: ${t.element._2}").mkString("{", ", ", "}")

    override def offset(i: Int): Int = if ((i & 1) > 0) elements(i / 2).element._1 else elements(i / 2).offset

    override val values: IndexedSeq[Element] = elements.flatMap(e => IndexedSeq(e.path, e.element._2))

    override def size: Int = end - start
  }

  case class Concatenation(start: Int, head: Element, last: Seq[(Int, Element)], end: Int) extends ElementCollection("Concatenation") {
    override def offset(index: Int): Int = if (index > 0) last(index - 1)._1 else start // TODO: Performance

    override val values: IndexedSeq[Element] = head +: last.map(_._2).toIndexedSeq

    def children: Seq[(Int, Element)] = (start, head) +: last

    override def size: Int = end - start
  }

  sealed trait Element {
    def size: Int = value.length

    def value: String
  }

  sealed abstract class ElementCollection(name: String) extends Element {
    def offset(index: Int): Int

    def values: IndexedSeq[Element]

    override lazy val value: String = values.map(_.value).mkString

    override def toString: String = values.mkString(name + "(", ", ", ")")
  }

  case class ObjectElement(offset: Int, path: PathExpression, element: (Int, Element))

  case class ListElement(offset: Int, element: Element)

}