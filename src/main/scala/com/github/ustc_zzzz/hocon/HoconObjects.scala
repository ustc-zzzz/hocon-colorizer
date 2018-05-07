package com.github.ustc_zzzz.hocon

import com.github.ustc_zzzz.hocon.HoconObjects.{Element, ElementCollection}

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

  case class FieldPathPart(value: String) extends Element

  case class FieldPath(values: Traversable[Element]) extends ElementCollection(values, "FieldPath") {
    override lazy val value: String = values.map(_.value).mkString(".")
  }

  case class Concatenation(values: Traversable[Element]) extends ElementCollection(values, "Concatenation")

  case class SubstitutionWeak(values: FieldPath) extends ElementCollection(values.values, "SubstitutionWeak") {
    override lazy val value: String = "${?" + values.value + "}"
  }

  case class SubstitutionStrong(values: FieldPath) extends ElementCollection(values.values, "SubstitutionStrong") {
    override lazy val value: String = "${" + values.value + "}"
  }

  case class List(values: Traversable[Element]) extends ElementCollection(values, "List") {
    override lazy val value: String = values.map(_.value).mkString("[", ", ", "]")
  }

  case class Object(values: Traversable[(FieldPath, Element)]) extends ElementCollection(values.map(_._2), "") {
    override lazy val value: String = values.map(t => s"${t._1.value}: ${t._2.value}").mkString("{", ", ", "}")

    override def toString: String = values.map(t => s"${t._1} -> ${t._2}").mkString("Object(", ", ", ")")
  }

  sealed trait Element {
    def value: String
  }

  sealed abstract class ElementCollection(values: Traversable[Element], name: String) extends Element {
    override lazy val value: String = values.map(_.value).mkString

    override def toString: String = values.mkString(name + "(", ", ", ")")
  }

}