package com.github.ustc_zzzz.hocon

import fastparse.all._

/**
  * @author ustc_zzzz
  */
// noinspection SpellCheckingInspection
object HoconParsers {

  import com.github.ustc_zzzz.hocon.HoconObjects._

  private val digits: Parser[_] = P(CharsWhileIn(strings = '0' to '9'))
  private val numeric: Parser[_] = P("0" | (CharIn(strings = '1' to '9') ~ digits.?))
  private val exponent: Parser[_] = P(CharIn(strings = "eE") ~ CharIn(strings = "+-").? ~ digits)
  private val unicode: Parser[_] = P("u" ~ CharIn(strings = '0' to '9', 'a' to 'f', 'A' to 'F').rep(exactly = 4))

  private val timeUnitSuffixAbbr: Parser[_] = P("ns" | "us" | "ms" | "s" | "m" | "h" | "d")
  private val timeUnitSuffix: Parser[_] = P(("nanosecond" | "microsecond" | "millisecond" | "second" | "minute" | "hour" | "day") ~ "s".?)

  private val spaces: Parser[_] = P(CharsWhile(c => c.isWhitespace && c != '\n'))
  private val comment: Parser[_] = P(("//" | "#") ~ CharsWhile(_ != '\n', min = 0))
  private val spacesCrossLines: Parser[_] = P((CharsWhile(c => c.isWhitespace) | comment).rep())

  private val valueSeparator: Parser[_] = P((comment.? ~ "\n" ~ spacesCrossLines.? ~ ",".? ~ spacesCrossLines) | ("," ~/ spacesCrossLines))

  private val escape: Parser[_] = P("\\" ~ (CharIn(strings = "\"\\\\/bfnrt") | unicode))
  private val quotedChar: Parser[_] = P(CharsWhile(c => c != '\"' && c != '\\') | escape)
  private val unquotedChar: Parser[_] = P(CharsWhile(c => !c.isWhitespace && !"$\"{}[]:=,+#`^?!@*&\\".contains(c)))
  private val rawStringChar: Parser[_] = P(CharsWhile(_ != '\"').~/ | "\"" ~ (CharsWhile(_ != '\"').~/ | "\"" ~ CharsWhile(_ != '\"').~/))

  private val nullPointerValue: Parser[NullPointer] = P("null").!.map(NullPointer)
  private val number: Parser[Number] = P("-".? ~ numeric ~ P("." ~ digits).? ~ exponent.?).!.map(Number)
  private val boolean: Parser[Boolean] = P("true" | "false" | "yes" | "no" | "on" | "off").!.map(Boolean)
  private val timeUnit: Parser[TimeUnit] = P(numeric ~ spaces.rep ~ (timeUnitSuffix | timeUnitSuffixAbbr)).!.map(TimeUnit)

  private val quotedString: Parser[QuotedString] = P("\"" ~/ quotedChar.rep ~ "\"").!.map(QuotedString)
  private val unquotedString: Parser[UnquotedString] = P(unquotedChar.rep(min = 1)).!.map(UnquotedString)
  private val rawString: Parser[MultilineString] = P("\"\"\"" ~/ rawStringChar.rep ~ "\"".rep(min = 3)).!.map(MultilineString)

  private val stringValue: Parser[Element] = P(rawString | quotedString | spaces.!.map(Space) | unquotedString)
  private val stringLikeValue: Parser[Element] = P(nullPointerValue | boolean | timeUnit | number | stringValue)

  private val fieldPathElement: Parser[String] = P(quotedString | unquotedString).map(_.value)
  private val fieldPathPart: Parser[FieldPathPart] = P(fieldPathElement).map(s => FieldPathPart(s.mkString))
  private val fieldPathExpression: Parser[FieldPath] = P(fieldPathPart.rep(min = 1, sep = P("."))).map(FieldPath)

  private val substitutionWeak: Parser[SubstitutionWeak] = P("${?" ~ fieldPathExpression ~ "}").map(SubstitutionWeak)
  private val substitutionStrong: Parser[SubstitutionStrong] = P("${" ~ fieldPathExpression ~ "}").map(SubstitutionStrong)

  private val substitution: Parser[Element] = P(substitutionWeak | substitutionStrong)
  private val stringRepeatition: Parser[Seq[Element]] = P(substitution | stringValue).rep
  private val listRepeatition: Parser[Seq[Element]] = P(substitution | listValue).rep(sep = spaces.?)
  private val objectRepeatition: Parser[Seq[Element]] = P(substitution | objectValue).rep(sep = spaces.?)

  private val fieldAppendValue: Parser[Concatenation] = P(spacesCrossLines ~ "+=" ~/ spacesCrossLines ~ values).map(t => Concatenation(t._3))
  private val fieldObjectValue: Parser[Concatenation] = P(spacesCrossLines ~ objectValue ~ objectRepeatition).map(t => Concatenation(t._2 +: t._3))
  private val fieldValue: Parser[Concatenation] = P(spacesCrossLines ~ CharIn(strings = ":=") ~/ spacesCrossLines ~ values).map(t => Concatenation(t._3))

  private val stringValues: Parser[Seq[Element]] = P(stringLikeValue ~ stringRepeatition).map(t => t._1 +: t._2)
  private val listValues: Parser[Seq[Element]] = P(listValue ~ spaces.? ~ listRepeatition).map(t => t._1 +: t._3)
  private val objectValues: Parser[Seq[Element]] = P(objectValue ~ spaces.? ~ objectRepeatition).map(t => t._1 +: t._3)
  private val substitutions: Parser[Seq[Element]] = P(substitution ~ spaces.? ~ values.?).map(t => t._1 +: t._3.getOrElse(Seq.empty))

  private val field: Parser[(FieldPath, Concatenation)] = P(fieldPathExpression ~/ (fieldValue | fieldAppendValue | fieldObjectValue))
  private val fields: Parser[Seq[(FieldPath, Concatenation)]] = P(spacesCrossLines ~ field.rep(sep = valueSeparator) ~ spacesCrossLines).map(_._2)
  private val elements: Parser[Seq[Concatenation]] = P(spacesCrossLines ~ values.map(Concatenation).rep(sep = valueSeparator) ~ spacesCrossLines).map(_._2)

  private def rootValue: Parser[Element] = P(objectValue | listValue | fields.map(Object))

  private def listValue: Parser[List] = P("[" ~/ elements ~ "]" ~ spaces.?).map(t => List(t._1))

  private def objectValue: Parser[Object] = P("{" ~/ fields ~ "}" ~ spaces.?).map(t => Object(t._1))

  private def values: Parser[Seq[Element]] = P(substitutions | stringValues | listValues | objectValues)

  def root: Parser[Element] = P(Start ~ spacesCrossLines ~ rootValue ~ spacesCrossLines ~ End).map(t => t._2)
}