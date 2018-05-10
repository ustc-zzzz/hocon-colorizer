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

  case class List(start: Int, elements: IndexedSeq[ListElementPart], end: Int) extends ElementCollection("List") {
    override def toString: String = elements.map(element => s"${element.value}").mkString("[", ", ", "]")

    override val values: IndexedSeq[Element] = elements.map(_.values.head)

    override lazy val value: String = elements.mkString("[", ", ", "]")

    override def offset(index: Int): Int = elements(index).offset(0)

    override def size: Int = end - start
  }

  case class Object(start: Int, elements: IndexedSeq[ObjectElementPart], end: Int) extends ElementCollection("Object") {
    override lazy val value: String = elements.map(_.value).mkString("{", ", ", "}")

    override def toString: String = elements.mkString("Object(", ", ", ")")

    override val values: IndexedSeq[Element] = elements.flatMap(_.values)

    override def offset(i: Int): Int = elements(i / 2).offset(i % 2)

    override def size: Int = end - start
  }

  case class ListElement(offset: Int, element: Element) extends ListElementPart {
    override def values: IndexedSeq[Element] = IndexedSeq(element)

    override def offset(index: Int): Int = offset
  }

  case class StringInclusion(offset: Int, element: QuotedString) extends InclusionElement(offset, element) {
    override def toString: String = values.mkString("StringInclusion(", ", ", ")")

    override lazy val value: String = s"include ${element.value}"
  }

  case class UrlInclusion(offset: Int, element: InclusionElement) extends InclusionElement(offset, element) {
    override lazy val value: String = s"include url(${element.value.substring(8)})"

    override def toString: String = values.mkString("UrlInclusion(", ", ", ")")
  }

  case class FileInclusion(offset: Int, element: InclusionElement) extends InclusionElement(offset, element) {
    override lazy val value: String = s"include file(${element.value.substring(8)})"

    override def toString: String = values.mkString("FileInclusion(", ", ", ")")
  }

  case class ClassPathInclusion(offset: Int, element: InclusionElement) extends InclusionElement(offset, element) {
    override lazy val value: String = s"include classpath(${element.value.substring(8)})"

    override def toString: String = values.mkString("ClassPathInclusion(", ", ", ")")
  }

  case class RequirementInclusion(offset: Int, element: InclusionElement) extends InclusionElement(offset, element) {
    override lazy val value: String = s"include required(${element.value.substring(8)})"

    override def toString: String = values.mkString("RequirementInclusion(", ", ", ")")
  }

  case class ObjectElement(offset: Int, fieldPath: PathExpression, fieldValue: (Int, Element)) extends ObjectElementPart {
    override def values: IndexedSeq[Element] = IndexedSeq(fieldPath, fieldValue._2)

    override def offset(i: Int): Int = if (i > 0) fieldValue._1 else offset
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

  sealed abstract class ListElementPart extends ElementCollection("ListElementPart") {
    override def toString: String = values.head.toString

    override lazy val value: String = values.head.value
  }

  sealed abstract class ObjectElementPart extends ElementCollection("ObjectElementPart") {
    override lazy val value: String = s"${values.head.value}: ${values.last.value}"

    override def toString: String = s"${values.head} -> ${values.last}"
  }

  sealed abstract class InclusionElement(offset: Int, element: Element) extends ObjectElementPart {
    override def toString: String = values.mkString("InclusionElement(", ", ", ")")

    override val values: IndexedSeq[Element] = IndexedSeq(element)

    override def offset(i: Int): Int = offset
  }

}