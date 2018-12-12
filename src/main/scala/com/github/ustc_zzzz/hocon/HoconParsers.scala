package com.github.ustc_zzzz.hocon

import fastparse.all._

/**
  * @author ustc_zzzz
  */
// noinspection ForwardReference
object HoconParsers {

  import com.github.ustc_zzzz.hocon.HoconObjects._
  import com.github.ustc_zzzz.hocon.HoconTokens._

  // spaces

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
    (spacesMultiLine ~ (",".!.map(Sep) ~ spacesMultiLine).?).map(SepWithSpaces.tupled)
  }

  private val moreFields: Parser[Seq[(ObjectElementPart, SepWithSpaces)]] = P {
    ((inclusion | field) ~ (valueSeparator ~ moreFields | valueSeparator.map((_, Nil)))).map(t => (t._1, t._2._1) +: t._2._2)
  }
  private val moreElements: Parser[Seq[(ListElementPart, SepWithSpaces)]] = P {
    (values.map(ListElement) ~ (valueSeparator ~ moreElements | valueSeparator.map((_, Nil)))).map(t => (t._1, t._2._1) +: t._2._2)
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

  @annotation.tailrec def printError(stack: IndexedSeq[fastparse.core.Frame], pointer: Int = 1): (Int, String) = {
    val fastparse.core.Frame(index, parser) = stack(stack.size - pointer)
    parser match {
      case `root` => (index, "Invalid root object syntax. Expect an object, a list, or a field")
      case `objectValue` => (index, "Invalid object syntax. An object should consist of fields surrounded by { and }")
      case `listValue` => (index, "Invalid list syntax. A list should consist of values surrounded by [ and ]")
      case `rootObjectValue` => (index, "Invalid root object syntax. Braces can only be omitted when it is a root object")
      case `moreElements` => (index, "Invalid list part syntax. Expect a list element")
      case `moreFields` => (index, "Invalid object part syntax. Expect an object field")
      case `valueSeparator` => (index, "Invalid value separator syntax. Please insert a comma or just a line break")
      case `values` => (index, "Invalid value syntax. Expect a substitution, a string, a list, or an object")
      case `field` => (index, "Invalid object field syntax. Please ensure that every field path has a corresponding value")
      case `inclusion` => (index, "Invalid inclusion syntax. Please check if the main part of inclusion is valid")
      case `fieldValue` => (index, "Invalid object field syntax. Separators should be : or = for field value definition")
      case `fieldAppendValue` => (index, "Invalid object field syntax. Separators should be += if it is wanted to append values")
      case `fieldObjectValue` => (index, "Invalid object field syntax. Separators can only be omitted when the value is an object")
      case `substitutions` => (index, "Invalid value concatenation syntax. Expect a substitution, a string, a list, or an object")
      case `stringValues` => (index, "Invalid string concatenation syntax. Expect a substitution, or a string")
      case `objectValues` => (index, "Invalid object concatenation syntax. Expect a substitution, or an object")
      case `listValues` => (index, "Invalid list concatenation syntax. Expect a substitution, or a list")
      case `objectRepetition` => (index, "Invalid object concatenation part syntax. Expect a substitution, or an object")
      case `listRepetition` => (index, "Invalid list concatenation part syntax. Expect a substitution, or a list")
      case `valueRepetition` => (index, "Invalid value concatenation part syntax. Expect a substitution, a string, a list, or an object")
      case `stringRepetition` => (index, "Invalid string concatenation part syntax. Expect a substitution, or a string")
      case `substitution` => (index, "Invalid substitution syntax. Expect a substitution in the form of ${<field path>}")
      case `substitutionOptional` => (index, "Invalid substitution syntax. Expect a substitution in the form of ${?<field path>}")
      case `fieldPathExpression` => (index, "Invalid path expression syntax. Expect a path expression for field key")
      case `stringLikeValue` => (index, "Invalid primitive value syntax. Expect a null, a boolean, a time unit, a number, or a string")
      case `stringOnlyValue` => (index, "Invalid string syntax. Expect a multiline string, a quoted string, or an unquoted string")
      case _ => printError(stack, pointer + 1)
    }
  }

  val root: Parser[Root] = P {
    (Start ~ spacesMultiLine ~ (objectValue | listValue | rootObjectValue | Fail) ~ spacesMultiLineEnd ~ End).map(Root.tupled)
  }
}
