package com.github.ustc_zzzz.hocon

import fastparse.all._

import scala.annotation.tailrec


/**
  * @author ustc_zzzz
  */
// noinspection ForwardReference
object HoconParsers {

  import com.github.ustc_zzzz.hocon.HoconTokens._
  import com.github.ustc_zzzz.hocon.HoconObjects._

  // token compounds

  private val stringOnlyValue: Parser[Element] = P {
    rawString | quotedString | spaces | unquotedString
  }
  private val stringLikeValue: Parser[Element] = P {
    nullPointerValue | boolean | timeUnit | number | stringOnlyValue
  }

  private val fieldPathExpression: Parser[PathExpression] = P {
    fieldPathElement.rep(min = 1, sep = P(".")).map(PathExpression)
  }

  private val substitutionOptional: Parser[SubstitutionWeak] = P {
    ("${?" ~/ fieldPathExpression ~ "}").map(SubstitutionWeak)
  }
  private val substitution: Parser[SubstitutionStrong] = P {
    ("${" ~/ fieldPathExpression ~ "}").map(SubstitutionStrong)
  }

  private val stringRepetition: Parser[Seq[Element]] = P {
    (substitutionOptional | substitution | stringOnlyValue).rep
  }
  private val valueRepetition: Parser[Seq[Element]] = P {
    (spaces.? ~ values).map(s => s._1.toSeq ++ s._2.elements).?.map(_.getOrElse(Seq.empty))
  }
  private val listRepetition: Parser[Seq[Element]] = P {
    (spaces.? ~ (substitutionOptional | substitution | listValue)).rep.map(_.flatMap(s => s._1.toSeq :+ s._2))
  }
  private val objectRepetition: Parser[Seq[Element]] = P {
    (spaces.? ~ (substitutionOptional | substitution | objectValue)).rep.map(_.flatMap(s => s._1.toSeq :+ s._2))
  }

  private val spacesSingleLine: Parser[SpacesSingleLine] = P {
    (spaces.? ~ Index).map(s => SpacesSingleLine(s._1, s._2, None))
  }
  private val spacesSingleLineEnd: Parser[SpacesSingleLine] = P {
    (spaces.? ~ Index ~ comment.?).map(SpacesSingleLine.tupled)
  }
  private val spacesSingleLineWithLineBreak: Parser[SpacesSingleLine] = P {
    (spaces.? ~ Index ~ comment.? ~ "\n").map(SpacesSingleLine.tupled)
  }

  private val spacesMultiLine: Parser[SpacesMultiLine] = P {
    (spacesSingleLineWithLineBreak.rep ~ spacesSingleLine).map(s => SpacesMultiLine(s._1 :+ s._2))
  }
  private val spacesMultiLineEnd: Parser[SpacesMultiLine] = P {
    (spacesSingleLineWithLineBreak.rep ~ spacesSingleLineEnd).map(s => SpacesMultiLine(s._1 :+ s._2))
  }

  private val listValues: Parser[Concat] = P {
    (listValue ~/ listRepetition).map(s => Concat(s._1 +: s._2, noString = true))
  }
  private val objectValues: Parser[Concat] = P {
    (objectValue ~/ objectRepetition).map(s => Concat(s._1 +: s._2, noString = true))
  }
  private val stringValues: Parser[Concat] = P {
    (stringLikeValue ~/ stringRepetition).map(s => Concat(s._1 +: s._2, noString = false))
  }
  private val substitutions: Parser[Concat] = P {
    ((substitutionOptional | substitution) ~/ valueRepetition).map(s => Concat(s._1 +: s._2, noString = false))
  }

  private val fieldObjectValue: Parser[(SepWithSpaces, Concat)] = P {
    (spacesMultiLine ~ objectValues).map(t => (SepWithSpaces(t._1, None), t._2))
  }
  private val fieldAppendValue: Parser[(SepWithSpaces, Concat)] = P {
    (spacesMultiLine ~ "+=".!.map(Sep) ~/ spacesMultiLine ~ values).map(t => (SepWithSpaces(t._1, Some(t._2, t._3)), t._4))
  }
  private val fieldValue: Parser[(SepWithSpaces, Concat)] = P {
    (spacesMultiLine ~ CharIn(":=").!.map(Sep) ~/ spacesMultiLine ~ values).map(t => (SepWithSpaces(t._1, Some(t._2, t._3)), t._4))
  }

  private val inclusion: Parser[Inclusion] = P {
    ("include" ~ spaces ~/ (optionalInclusion | requirementInclusion)).map(Inclusion.tupled)
  }
  private val field: Parser[ObjectElement] = P {
    (fieldPathExpression ~/ (fieldValue | fieldAppendValue | fieldObjectValue)).map(ObjectElement.tupled)
  }

  private val values: Parser[Concat] = P {
    substitutions | stringValues | listValues | objectValues
  }
  private val valueSeparator: Parser[SepWithSpaces] = P {
    valueSeparatorRequired | spacesMultiLine.map(SepWithSpaces(_, None))
  }
  private val valueSeparatorLast: Parser[SepWithSpaces] = P {
    valueSeparatorRequired | spacesMultiLine.map(SepWithSpaces(_, None))
  }
  private val valueSeparatorRequired: Parser[SepWithSpaces] = P {
    (spacesMultiLine ~ ("," ~ spacesMultiLine).map(Some(Sep(","), _))).map(SepWithSpaces.tupled)
  }

  private val moreFields: Parser[Seq[(ObjectElementPart, SepWithSpaces)]] = P {
    ((inclusion | field) ~ (valueSeparator ~ moreFields | valueSeparatorLast.map((_, Nil)))).map(t => (t._1, t._2._1) +: t._2._2)
  }
  private val moreElements: Parser[Seq[(ListElementPart, SepWithSpaces)]] = P {
    (values.map(ListElement) ~ (valueSeparator ~ moreElements | valueSeparatorLast.map((_, Nil)))).map(t => (t._1, t._2._1) +: t._2._2)
  }

  private val rootObjectValue: Parser[RootObject] = P {
    (spacesMultiLine ~/ moreFields.?.map(_.getOrElse(Nil))).map(RootObject.tupled)
  }
  private val listValue: Parser[List] = P {
    ("[" ~/ spacesMultiLine ~/ moreElements.?.map(_.getOrElse(Nil)) ~ "]").map(List.tupled)
  }
  private val objectValue: Parser[Object] = P {
    ("{" ~/ spacesMultiLine ~/ moreFields.?.map(_.getOrElse(Nil)) ~ "}").map(Object.tupled)
  }

  @tailrec def printError(stack: IndexedSeq[fastparse.core.Frame], pointer: Int = 1): (Int, String) = {
    val fastparse.core.Frame(index, parser) = stack(stack.size - pointer)
    parser match {
      case `root` =>
        (index, "Invalid root object syntax. Expecting an object, a list, or a field")
      case `inclusion` =>
        (index, "Invalid include syntax")
      case `field` =>
        (index, "Invalid object field syntax")
      case `fieldPathExpression` =>
        (index, "Invalid path expression syntax. Expecting a path expression for field key")
      case `fieldPathElement` =>
        (index, "Invalid path expression syntax. Expecting a quoted or unquoted string for path expression")
      case `listValue` =>
        (index, "Invalid list syntax. Expecting list elements")
      case `objectValue` =>
        (index, "Invalid object syntax. Expecting object fields")
      case `values` =>
        (index, "Invalid value syntax. Expecting a substitution, a string, a list, or an object")
      case `stringOnlyValue` =>
        (index, "Invalid string syntax. Expecting a multiline string, a quoted string, or an unquoted string")
      case `stringLikeValue` =>
        (index, "Invalid primitive value syntax. Expecting a null, a boolean, a time unit, a number, or a string")
      case `valueRepetition` =>
        (index, "Invalid value concatenation syntax. Expecting a substitution, a string, a list, an object, or nothing")
      case `stringRepetition` =>
        (index, "Invalid string concatenation syntax. Expecting a substitution, or a string")
      case `listRepetition` =>
        (index, "Invalid list concatenation syntax. Expecting a substitution, or a list")
      case `objectRepetition` =>
        (index, "Invalid object concatenation syntax. Expecting a substitution, or an object")
      case _ => printError(stack, pointer + 1)
    }
  }

  val root: Parser[Root] = P {
    (Start ~ spacesMultiLine ~ (objectValue | listValue | rootObjectValue | Fail) ~ spacesMultiLineEnd ~ End).map(Root.tupled)
  }
}
