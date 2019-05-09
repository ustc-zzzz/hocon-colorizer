package com.github.ustc_zzzz.hocon

import fastparse._
import fastparse.NoWhitespace._

/**
  * @author ustc_zzzz
  */
object HoconTokens {

  import com.github.ustc_zzzz.hocon.HoconObjects._

  // token parts

  private def digits[_: P]: P[_] = P(CharsWhileIn("0123456789"))

  private def numeric[_: P]: P[_] = P("0" | (CharIn("123456789") ~ digits.?))

  private def exponent[_: P]: P[_] = P("e" | "e+" | "e-" | "E" | "E+" | "E-" ~ digits)

  private def unicode[_: P]: P[_] = P("u" ~ CharIn("0123456789", "abcdef", "ABCDEF").rep(exactly = 4))

  private def commentPart[_: P]: P[_] = P(CharsWhile(_ != '\n', 0))

  private def spacesPart[_: P]: P[_] = P(CharsWhile {
    case '\n' => false // special handling for line breaks
    case ' ' | '\t' | '\uFEFF' => true // common whitespaces and utf-8 bom
    case '\u00A0' | '\u2007' | '\u202F' => true // characters mentioned in the spec
    case character => character.isWhitespace // use java.lang.Character in other cases
  })

  private def timeUnitSuffixAbbr[_: P]: P[_] = P {
    "ns" | "us" | "ms" | "s" | "m" | "h" | "d"
  }
  private def timeUnitSuffix[_: P]: P[_] = P {
    ("nanosecond" | "microsecond" | "millisecond" | "second" | "minute" | "hour" | "day") ~ "s".?
  }

  private def escapeChar[_: P]: P[_] = P {
    "\\" ~ (CharIn("\"\\/", "bfnrt") | unicode)
  }
  private def quotedChar[_: P]: P[_] = P {
    CharsWhile(c => c != '\"' && c != '\\' && !c.isControl) | escapeChar
  }
  private def unquotedChar[_: P]: P[_] = P {
    CharsWhile(c => !c.isWhitespace && !"$\"{}[]:=,+#`^?!@*&\\/".contains(c))
  }
  private def rawStringChar[_: P]: P[_] = P {
    CharsWhile(_ != '\"')./ | "\"" ~ (CharsWhile(_ != '\"')./ | "\"" ~ CharsWhile(_ != '\"')./)
  }

  private def urlInclusion[_: P]: P[_] = P {
    "url(" ~/ spacesPart.? ~ "\"" ~ quotedChar.rep ~ "\"" ~ spacesPart.? ~ ")"
  }
  private def fileInclusion[_: P]: P[_] = P {
    "file(" ~/ spacesPart.? ~ "\"" ~ quotedChar.rep ~ "\"" ~ spacesPart.? ~ ")"
  }
  private def classPathInclusion[_: P]: P[_] = P {
    "classpath(" ~/ spacesPart.? ~ "\"" ~ quotedChar.rep ~ "\"" ~ spacesPart.? ~ ")"
  }

  // tokens

  def spaces[_: P]: P[Spaces] = P {
    (Index ~ spacesPart.! ~ Index).map(Spaces.tupled).opaque("Spaces")
  }
  def comment[_: P]: P[Comment] = P {
    (("//" | "#").! ~ Index ~ spacesPart.?.! ~ Index ~ commentPart.!).map(Comment.tupled).opaque("Comment")
  }

  def nullPointerValue[_: P]: P[NullPointer] = P {
    "null".!.map(NullPointer).opaque("Null")
  }
  def boolean[_: P]: P[Boolean] = P {
    ("true" | "false" | "yes" | "no" | "on" | "off").!.map(Boolean).opaque("Boolean")
  }
  def number[_: P]: P[Number] = P {
    ("-".? ~ numeric ~ P("." ~ digits).? ~ exponent.?).!.map(Number).opaque("Number")
  }
  def timeUnit[_: P]: P[TimeUnit] = P {
    (numeric ~ spacesPart.? ~ (timeUnitSuffix | timeUnitSuffixAbbr)).!.map(TimeUnit).opaque("TimeUnit")
  }

  def quotedString[_: P]: P[QuotedString] = P {
    ("\"" ~/ quotedChar.rep ~ "\"").!.map(QuotedString).opaque("QuotedString")
  }
  def unquotedString[_: P]: P[UnquotedString] = P {
    (unquotedChar | "/" ~ !"/").rep(1).!.map(UnquotedString).opaque("UnquotedString")
  }
  def rawString[_: P]: P[MultilineString] = P {
    ("\"\"\"" ~/ rawStringChar.rep ~ "\"".rep(3)).!.map(MultilineString).opaque("MultilineString")
  }

  def fieldPathElement[_: P]: P[PathExpressionPart] = P {
    (quotedString | unquotedString).map(_.value).map(PathExpressionPart).opaque("PathExpression")
  }

  def optionalInclusion[_: P]: P[InclusionBodyPart] = P {
    (urlInclusion | fileInclusion | classPathInclusion | quotedString).!.map(InclusionBodyPart).opaque("Inclusion")
  }
  def requirementInclusion[_: P]: P[InclusionBodyPart] = P {
    ("required(" ~/ spacesPart.? ~ optionalInclusion ~ spacesPart.? ~ ")").!.map(InclusionBodyPart).opaque("Inclusion")
  }
}
