package com.github.ustc_zzzz.hocon

import fastparse._
import fastparse.NoWhitespace._

/**
  * @author ustc_zzzz
  */
// noinspection ForwardReference
object HoconParsers {

  import com.github.ustc_zzzz.hocon.HoconObjects._
  import com.github.ustc_zzzz.hocon.HoconTokens._

  // spaces

  private def spacesSingleLine[_: P]: P[SpacesSingleLine] = P {
    (spaces.? ~ Index).map(s => SpacesSingleLine(s._1, s._2, None))
  }
  private def spacesSingleLineEnd[_: P]: P[SpacesSingleLine] = P {
    (spaces.? ~ Index ~ comment.?).map(SpacesSingleLine.tupled)
  }
  private def spacesSingleLineWithLineBreak[_: P]: P[SpacesSingleLine] = P {
    (spaces.? ~ Index ~ comment.? ~ "\n").map(SpacesSingleLine.tupled)
  }

  private def spacesMultiLine[_: P]: P[SpacesMultiLine] = P {
    (spacesSingleLineWithLineBreak.rep ~ spacesSingleLine).map(s => SpacesMultiLine(s._1 :+ s._2))
  }
  private def spacesMultiLineEnd[_: P]: P[SpacesMultiLine] = P {
    (spacesSingleLineWithLineBreak.rep ~ spacesSingleLineEnd).map(s => SpacesMultiLine(s._1 :+ s._2))
  }

  // token compounds

  private def stringOnlyValue[_: P]: P[Element] = P {
    rawString | quotedString | spaces | unquotedString
  }
  private def stringLikeValue[_: P]: P[Element] = P {
    nullPointerValue | boolean | timeUnit | number | stringOnlyValue
  }

  private def fieldPathExpression[_: P]: P[PathExpression] = P {
    fieldPathElement.rep(min = 1, sep = P(".")).map(PathExpression)
  }

  private def substitutionOptional[_: P]: P[SubstitutionWeak] = P {
    ("${?" ~/ fieldPathExpression ~ "}").map(SubstitutionWeak)
  }
  private def substitution[_: P]: P[SubstitutionStrong] = P {
    ("${" ~/ fieldPathExpression ~ "}").map(SubstitutionStrong)
  }

  private def stringRepetition[_: P]: P[Seq[Element]] = P {
    (substitutionOptional | substitution | stringOnlyValue).rep
  }
  private def valueRepetition[_: P]: P[Seq[Element]] = P {
    (spaces.? ~ values).map(s => s._1.toSeq ++ s._2.elements).?.map(_.getOrElse(Seq.empty))
  }
  private def listRepetition[_: P]: P[Seq[Element]] = P {
    (spaces.? ~ (substitutionOptional | substitution | listValue)).rep.map(_.flatMap(s => s._1.toSeq :+ s._2))
  }
  private def objectRepetition[_: P]: P[Seq[Element]] = P {
    (spaces.? ~ (substitutionOptional | substitution | objectValue)).rep.map(_.flatMap(s => s._1.toSeq :+ s._2))
  }

  private def listValues[_: P]: P[Concat] = P {
    (listValue ~/ listRepetition).map(s => Concat(s._1 +: s._2, noString = true))
  }
  private def objectValues[_: P]: P[Concat] = P {
    (objectValue ~/ objectRepetition).map(s => Concat(s._1 +: s._2, noString = true))
  }
  private def stringValues[_: P]: P[Concat] = P {
    (stringLikeValue ~/ stringRepetition).map(s => Concat(s._1 +: s._2, noString = false))
  }
  private def substitutions[_: P]: P[Concat] = P {
    ((substitutionOptional | substitution) ~/ valueRepetition).map(s => Concat(s._1 +: s._2, noString = false))
  }

  private def fieldObjectValue[_: P]: P[(SepWithSpaces, Concat)] = P {
    (spacesMultiLine ~ objectValues).map(t => (SepWithSpaces(t._1, None), t._2))
  }
  private def fieldAppendValue[_: P]: P[(SepWithSpaces, Concat)] = P {
    (spacesMultiLine ~ "+=".!.map(Sep) ~/ spacesMultiLine ~ values).map(t => (SepWithSpaces(t._1, Some(t._2, t._3)), t._4))
  }
  private def fieldValue[_: P]: P[(SepWithSpaces, Concat)] = P {
    (spacesMultiLine ~ CharIn(":=").!.map(Sep) ~/ spacesMultiLine ~ values).map(t => (SepWithSpaces(t._1, Some(t._2, t._3)), t._4))
  }

  private def inclusion[_: P]: P[Inclusion] = P {
    ("include" ~ spaces ~/ (optionalInclusion | requirementInclusion)).map(Inclusion.tupled)
  }
  private def field[_: P]: P[ObjectElement] = P {
    (fieldPathExpression ~/ (fieldValue | fieldAppendValue | fieldObjectValue)).map(ObjectElement.tupled)
  }

  private def values[_: P]: P[Concat] = P {
    substitutions | stringValues | listValues | objectValues
  }
  private def valueSeparator[_: P]: P[SepWithSpaces] = P {
    (spacesMultiLine ~ (",".!.map(Sep) ~ spacesMultiLine).?).map(SepWithSpaces.tupled)
  }

  private def moreFields[_: P]: P[Seq[(ObjectElementPart, SepWithSpaces)]] = P {
    ((inclusion | field) ~ (valueSeparator ~ moreFields | valueSeparator.map((_, Nil)))).map(t => (t._1, t._2._1) +: t._2._2)
  }
  private def moreElements[_: P]: P[Seq[(ListElementPart, SepWithSpaces)]] = P {
    (values.map(ListElement) ~ (valueSeparator ~ moreElements | valueSeparator.map((_, Nil)))).map(t => (t._1, t._2._1) +: t._2._2)
  }

  private def rootObjectValue[_: P]: P[RootObject] = P {
    (spacesMultiLine ~/ moreFields.?.map(_.getOrElse(Nil))).map(RootObject.tupled)
  }
  private def listValue[_: P]: P[List] = P {
    ("[" ~/ spacesMultiLine ~/ moreElements.?.map(_.getOrElse(Nil)) ~ "]").map(List.tupled)
  }
  private def objectValue[_: P]: P[Object] = P {
    ("{" ~/ spacesMultiLine ~/ moreFields.?.map(_.getOrElse(Nil)) ~ "}").map(Object.tupled)
  }

  @annotation.tailrec def printError(stack: IndexedSeq[(String, Int)], pointer: Int = 1): (Int, String) = {
    val (parser, index) = stack(stack.size - pointer)
    parser match {
      case "root" => (index, "Invalid root object syntax. Expect an object, a list, or a field")
      case "objectValue" => (index, "Invalid object syntax. An object should consist of fields surrounded by { and }")
      case "listValue" => (index, "Invalid list syntax. A list should consist of values surrounded by [ and ]")
      case "rootObjectValue" => (index, "Invalid root object syntax. Braces can only be omitted when it is a root object")
      case "moreElements" => (index, "Invalid list part syntax. Expect a list element")
      case "moreFields" => (index, "Invalid object part syntax. Expect an object field")
      case "valueSeparator" => (index, "Invalid value separator syntax. Please insert a comma or just a line break")
      case "values" => (index, "Invalid value syntax. Expect a substitution, a string, a list, or an object")
      case "field" => (index, "Invalid object field syntax. Please ensure that every field path has a corresponding value")
      case "inclusion" => (index, "Invalid inclusion syntax. Please check if the main part of inclusion is valid")
      case "fieldValue" => (index, "Invalid object field syntax. Separators should be : or = for field value definition")
      case "fieldAppendValue" => (index, "Invalid object field syntax. Separators should be += if it is wanted to append values")
      case "fieldObjectValue" => (index, "Invalid object field syntax. Separators can only be omitted when the value is an object")
      case "substitutions" => (index, "Invalid value concatenation syntax. Expect a substitution, a string, a list, or an object")
      case "stringValues" => (index, "Invalid string concatenation syntax. Expect a substitution, or a string")
      case "objectValues" => (index, "Invalid object concatenation syntax. Expect a substitution, or an object")
      case "listValues" => (index, "Invalid list concatenation syntax. Expect a substitution, or a list")
      case "objectRepetition" => (index, "Invalid object concatenation part syntax. Expect a substitution, or an object")
      case "listRepetition" => (index, "Invalid list concatenation part syntax. Expect a substitution, or a list")
      case "valueRepetition" => (index, "Invalid value concatenation part syntax. Expect a substitution, a string, a list, or an object")
      case "stringRepetition" => (index, "Invalid string concatenation part syntax. Expect a substitution, or a string")
      case "substitution" => (index, "Invalid substitution syntax. Expect a substitution in the form of ${<field path>}")
      case "substitutionOptional" => (index, "Invalid substitution syntax. Expect a substitution in the form of ${?<field path>}")
      case "fieldPathExpression" => (index, "Invalid path expression syntax. Expect a path expression for field key")
      case "stringLikeValue" => (index, "Invalid primitive value syntax. Expect a null, a boolean, a time unit, a number, or a string")
      case "stringOnlyValue" => (index, "Invalid string syntax. Expect a multiline string, a quoted string, or an unquoted string")
      case _ => printError(stack, pointer + 1)
    }
  }

  def root[_: P]: P[Root] = P {
    (Start ~ spacesMultiLine ~ (objectValue | listValue | rootObjectValue | Fail) ~ spacesMultiLineEnd ~ End).map(Root.tupled)
  }
}
