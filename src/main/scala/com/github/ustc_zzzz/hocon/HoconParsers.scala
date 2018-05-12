package com.github.ustc_zzzz.hocon

import fastparse.all._

/**
  * @author ustc_zzzz
  */
// noinspection ForwardReference
// noinspection SpellCheckingInspection
object HoconParsers {

  import com.github.ustc_zzzz.hocon.HoconObjects._

  import scala.language.existentials

  private val digits: Parser[_] = P {
    CharsWhileIn(strings = '0' to '9')
  }
  private val numeric: Parser[_] = P {
    "0" | (CharIn(strings = '1' to '9') ~ digits.?)
  }
  private val exponent: Parser[_] = P {
    CharIn(strings = "eE") ~ CharIn(strings = "+-").? ~ digits
  }
  private val unicode: Parser[_] = P {
    "u" ~ CharIn(strings = '0' to '9', 'a' to 'f', 'A' to 'F').rep(exactly = 4)
  }

  private val timeUnitSuffixAbbr: Parser[_] = P {
    "ns" | "us" | "ms" | "s" | "m" | "h" | "d"
  }
  private val timeUnitSuffix: Parser[_] = P {
    ("nanosecond" | "microsecond" | "millisecond" | "second" | "minute" | "hour" | "day") ~ "s".?
  }

  private val spaces: Parser[_] = P {
    CharsWhile(c => c.isWhitespace && c != '\n')
  }
  private val comment: Parser[_] = P {
    ("//" | "#") ~ CharsWhile(_ != '\n', min = 0)
  }
  private val spacesCrossLines: Parser[_] = P {
    (CharsWhile(c => c.isWhitespace) | comment).rep
  }

  private val escapeChar: Parser[_] = P {
    "\\" ~ (CharIn(strings = "\"\\\\/bfnrt") | unicode)
  }
  private val quotedChar: Parser[_] = P {
    CharsWhile(c => c != '\"' && c != '\\' && !c.isControl) | escapeChar
  }
  private val unquotedChar: Parser[_] = P {
    CharsWhile(c => !c.isWhitespace && !"$\"{}[]:=,+#`^?!@*&\\".contains(c))
  }
  private val rawStringChar: Parser[_] = P {
    CharsWhile(_ != '\"').~/ | "\"" ~ (CharsWhile(_ != '\"').~/ | "\"" ~ CharsWhile(_ != '\"').~/)
  }

  private val nullPointerValue: Parser[NullPointer] = P {
    "null".!.map(NullPointer)
  }
  private val boolean: Parser[Boolean] = P {
    ("true" | "false" | "yes" | "no" | "on" | "off").!.map(Boolean)
  }
  private val number: Parser[Number] = P {
    ("-".? ~ numeric ~ P("." ~ digits).? ~ exponent.?).!.map(Number)
  }
  private val timeUnit: Parser[TimeUnit] = P {
    (numeric ~ spaces.rep ~ (timeUnitSuffix | timeUnitSuffixAbbr)).!.map(TimeUnit)
  }

  private val unquotedString: Parser[UnquotedString] = P {
    unquotedChar.rep(min = 1).!.map(UnquotedString)
  }
  private val quotedString: Parser[QuotedString] = P {
    ("\"" ~/ quotedChar.rep ~ "\"").!.map(QuotedString)
  }
  private val rawString: Parser[MultilineString] = P {
    ("\"\"\"" ~/ rawStringChar.rep ~ "\"".rep(min = 3)).!.map(MultilineString)
  }

  private val stringOnlyValue: Parser[Element] = P {
    rawString | quotedString | spaces.!.map(Space) | unquotedString
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

  private val stringRepetition: Parser[Seq[(Int, Element)]] = P {
    (Index ~ (substitutionOptional | substitution | stringOnlyValue)).rep
  }
  private val valueRepetition: Parser[Seq[(Int, Element)]] = P {
    (spaces.? ~ values.map(_.children)).map(_._2).?.map(_.getOrElse(Seq.empty))
  }
  private val listRepetition: Parser[Seq[(Int, Element)]] = P {
    (spaces.? ~ (Index ~ (substitutionOptional | substitution | listValue))).map(_._2).rep
  }
  private val objectRepetition: Parser[Seq[(Int, Element)]] = P {
    (spaces.? ~ (Index ~ (substitutionOptional | substitution | objectValue))).map(_._2).rep
  }

  private val listValues: Parser[Concatenation] = P {
    (Index ~ listValue ~/ listRepetition ~ Index).map(Concatenation.tupled)
  }
  private val objectValues: Parser[Concatenation] = P {
    (Index ~ objectValue ~/ objectRepetition ~ Index).map(Concatenation.tupled)
  }
  private val stringValues: Parser[Concatenation] = P {
    (Index ~ stringLikeValue ~/ stringRepetition ~ Index).map(Concatenation.tupled)
  }
  private val substitutions: Parser[Concatenation] = P {
    (Index ~ (substitutionOptional | substitution) ~/ valueRepetition ~ Index).map(Concatenation.tupled)
  }

  private val stringInclusion: Parser[StringInclusion] = P {
    (Index ~ quotedString).map(StringInclusion.tupled)
  }
  private val urlInclusion: Parser[UrlInclusion] = P {
    (Index ~ "url(" ~/ spaces.? ~ stringInclusion ~ spaces.? ~ ")").map(t => UrlInclusion(t._1, t._3))
  }
  private val fileInclusion: Parser[FileInclusion] = P {
    (Index ~ "file(" ~/ spaces.? ~ stringInclusion ~ spaces.? ~ ")").map(t => FileInclusion(t._1, t._3))
  }
  private val classPathInclusion: Parser[ClassPathInclusion] = P {
    (Index ~ "classpath(" ~/ spaces.? ~ stringInclusion ~ spaces.? ~ ")").map(t => ClassPathInclusion(t._1, t._3))
  }

  private val optionalInclusion: Parser[InclusionElement] = P {
    urlInclusion | fileInclusion | classPathInclusion | stringInclusion
  }
  private val requirementInclusion: Parser[RequirementInclusion] = P {
    (Index ~ "required(" ~/ spaces.? ~ optionalInclusion ~ spaces.? ~ ")").map(t => RequirementInclusion(t._1, t._3))
  }

  private val fieldObjectValue: Parser[(Int, Concatenation)] = P {
    (spacesCrossLines ~ Index ~ objectValues).map(t => (t._2, t._3))
  }
  private val fieldAppendValue: Parser[(Int, Concatenation)] = P {
    (spacesCrossLines ~ "+=" ~/ spacesCrossLines ~ Index ~ values).map(t => (t._3, t._4))
  }
  private val fieldValue: Parser[(Int, Concatenation)] = P {
    (spacesCrossLines ~ CharIn(strings = ":=") ~/ spacesCrossLines ~ Index ~ values).map(t => (t._3, t._4))
  }

  private val inclusion: Parser[InclusionElement] = P {
    ("include" ~/ spaces ~ (optionalInclusion | requirementInclusion)).map(_._2)
  }
  private val field: Parser[ObjectElement] = P {
    (Index ~ fieldPathExpression ~/ (fieldValue | fieldAppendValue | fieldObjectValue)).map(ObjectElement.tupled)
  }

  private val values: Parser[Concatenation] = P {
    substitutions | stringValues | listValues | objectValues
  }
  private val valueSeparator: Parser[Any] = P {
    spaces.? ~ ((comment.? ~ "\n" ~ spacesCrossLines ~ ",".? ~ spacesCrossLines) | ("," ~/ spacesCrossLines))
  }
  private val fields: Parser[IndexedSeq[ObjectElementPart]] = P {
    (spacesCrossLines ~ (inclusion | field).rep(sep = valueSeparator) ~ spacesCrossLines).map(_._2.toIndexedSeq)
  }
  private val elements: Parser[IndexedSeq[ListElementPart]] = P {
    (spacesCrossLines ~ (Index ~ values).map(ListElement.tupled).rep(sep = valueSeparator) ~ spacesCrossLines).map(_._2.toIndexedSeq)
  }

  private val rootObjectValue: Parser[Object] = P {
    (Index ~ fields ~ Index).map(Object.tupled)
  }
  private val listValue: Parser[List] = P {
    (Index ~ "[" ~/ elements ~ "]" ~ Index).map(List.tupled)
  }
  private val objectValue: Parser[Object] = P {
    (Index ~ "{" ~/ fields ~ "}" ~ Index).map(Object.tupled)
  }

  def printError(stack: IndexedSeq[fastparse.core.Frame], pointer: Int = 1): (Int, String) = {
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

  val root: Parser[Element] = P {
    (Start ~ spacesCrossLines ~ (objectValue | listValue | rootObjectValue | Fail) ~ spacesCrossLines ~ End).map(_._2)
  }
}