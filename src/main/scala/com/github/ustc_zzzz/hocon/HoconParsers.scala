package com.github.ustc_zzzz.hocon

import fastparse.all._

import scala.annotation.tailrec


/**
  * @author ustc_zzzz
  */
// noinspection ForwardReference
object HoconParsers {

  import com.github.ustc_zzzz.hocon.HoconObjects._

  // token parts

  private val digits: Parser[_] = P(CharsWhileIn(strings = '0' to '9'))

  private val numeric: Parser[_] = P("0" | (CharIn(strings = '1' to '9') ~ digits.?))

  private val exponent: Parser[_] = P(CharIn(strings = "eE") ~ CharIn(strings = "+-").? ~ digits)

  private val unicode: Parser[_] = P("u" ~ CharIn(strings = '0' to '9', 'a' to 'f', 'A' to 'F').rep(exactly = 4))

  private val blank: Parser[_] = P(CharsWhile {
    case '\n' => false // special handling for line breaks
    case ' ' | '\t' | '\uFEFF' => true // common whitespaces and utf-8 bom
    case '\u00A0' | '\u2007' | '\u202F' => true // characters mentioned in the spec
    case character => character.isWhitespace // use java.lang.Character in other cases
  })

  private val timeUnitSuffixAbbr: Parser[_] = P {
    "ns" | "us" | "ms" | "s" | "m" | "h" | "d"
  }
  private val timeUnitSuffix: Parser[_] = P {
    ("nanosecond" | "microsecond" | "millisecond" | "second" | "minute" | "hour" | "day") ~ "s".?
  }

  private val escapeChar: Parser[_] = P {
    "\\" ~ (CharIn(Seq('\"', '\\', '/', 'b', 'f', 'n', 'r', 't')) | unicode)
  }
  private val quotedChar: Parser[_] = P {
    CharsWhile(c => c != '\"' && c != '\\' && !c.isControl) | escapeChar
  }
  private val unquotedChar: Parser[_] = P {
    CharsWhile(c => !c.isWhitespace && !"$\"{}[]:=,+#`^?!@*&\\/".contains(c))
  }
  private val rawStringChar: Parser[_] = P {
    CharsWhile(_ != '\"').~/ | "\"" ~ (CharsWhile(_ != '\"').~/ | "\"" ~ CharsWhile(_ != '\"').~/)
  }

  // tokens

  private val spaces: Parser[Spaces] = P {
    (Index ~ blank.! ~ Index).map(Spaces.tupled).opaque("Spaces")
  }
  private val comment: Parser[Comment] = P {
    (("//" | "#").! ~ spaces.? ~ CharsWhile(_ != '\n', min = 0).!).map(Comment.tupled).opaque("Comment")
  }

  private val spacesSingleLine: Parser[(Option[Spaces], Int, Option[Comment])] = P {
    (spaces.? ~ Index).map(s => (s._1, s._2, None)).opaque("Spaces")
  }
  private val spacesSingleLineEnd: Parser[(Option[Spaces], Int, Option[Comment])] = P {
    (spaces.? ~ Index ~ comment.?).opaque("Spaces")
  }
  private val spacesSingleLineWithLineBreak: Parser[(Option[Spaces], Int, Option[Comment])] = P {
    (spaces.? ~ Index ~ comment.? ~ "\n").opaque("Spaces")
  }

  private val spacesMultiLine: Parser[SpacesMultiline] = P {
    (spacesSingleLineWithLineBreak.rep ~ spacesSingleLine).map(s => SpacesMultiline(s._1 :+ s._2)).opaque("Spaces")
  }
  private val spacesMultiLineEnd: Parser[SpacesMultiline] = P {
    (spacesSingleLineWithLineBreak.rep ~ spacesSingleLineEnd).map(s => SpacesMultiline(s._1 :+ s._2)).opaque("Spaces")
  }

  private val nullPointerValue: Parser[NullPointer] = P {
    "null".!.map(NullPointer).opaque("Null")
  }
  private val boolean: Parser[Boolean] = P {
    ("true" | "false" | "yes" | "no" | "on" | "off").!.map(Boolean).opaque("Boolean")
  }
  private val number: Parser[Number] = P {
    ("-".? ~ numeric ~ P("." ~ digits).? ~ exponent.?).!.map(Number).opaque("Number")
  }
  private val timeUnit: Parser[TimeUnit] = P {
    (numeric ~ spaces.rep ~ (timeUnitSuffix | timeUnitSuffixAbbr)).!.map(TimeUnit).opaque("TimeUnit")
  }

  private val quotedString: Parser[QuotedString] = P {
    ("\"" ~/ quotedChar.rep ~ "\"").!.map(QuotedString).opaque("QuotedString")
  }
  private val unquotedString: Parser[UnquotedString] = P {
    (unquotedChar | "/" ~ !"/").rep(min = 1).!.map(UnquotedString).opaque("UnquotedString")
  }
  private val rawString: Parser[MultilineString] = P {
    ("\"\"\"" ~/ rawStringChar.rep ~ "\"".rep(min = 3)).!.map(MultilineString).opaque("MultilineString")
  }

  // token compounds

  private val stringOnlyValue: Parser[Element] = P {
    rawString | quotedString | spaces | unquotedString
  }
  private val stringLikeValue: Parser[Element] = P {
    nullPointerValue | boolean | timeUnit | number | stringOnlyValue
  }

  private val fieldPathElement: Parser[String] = P {
    (quotedString | unquotedString).map(_.value)
  }
  private val fieldPathExpression: Parser[PathExpression] = P {
    fieldPathElement.map(PathExpressionPart).rep(min = 1, sep = P(".")).map(PathExpression)
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

  private val urlInclusion: Parser[_] = P {
    "url(" ~/ spaces.? ~ quotedString ~ spaces.? ~ ")"
  }
  private val fileInclusion: Parser[_] = P {
    "file(" ~/ spaces.? ~ quotedString ~ spaces.? ~ ")"
  }
  private val classPathInclusion: Parser[_] = P {
    "classpath(" ~/ spaces.? ~ quotedString ~ spaces.? ~ ")"
  }

  private val optionalInclusion: Parser[InclusionBodyPart] = P {
    (urlInclusion | fileInclusion | classPathInclusion | quotedString).!.map(InclusionBodyPart)
  }
  private val requirementInclusion: Parser[InclusionBodyPart] = P {
    ("required(" ~/ spaces.? ~ optionalInclusion ~ spaces.? ~ ")").!.map(InclusionBodyPart)
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

  private val fields: Parser[(SpacesMultiline, Option[(ObjectElementPart, Seq[(SepWithSpaces, ObjectElementPart)], SepWithSpaces)])] = P {
    spacesMultiLine ~/ ((inclusion | field) ~ (valueSeparator ~ (inclusion | field)).rep ~ valueSeparatorLast).?
  }
  private val elements: Parser[(SpacesMultiline, Option[(ListElementPart, Seq[(SepWithSpaces, ListElementPart)], SepWithSpaces)])] = P {
    spacesMultiLine ~/ (values.map(ListElement) ~ (valueSeparator ~ values.map(ListElement)).rep ~ valueSeparatorLast).?
  }

  private val rootObjectValue: Parser[RootObject] = P {
    fields.map(RootObject.tupled)
  }
  private val listValue: Parser[List] = P {
    ("[" ~/ elements ~ "]").map(List.tupled)
  }
  private val objectValue: Parser[Object] = P {
    ("{" ~/ fields ~ "}").map(Object.tupled)
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
