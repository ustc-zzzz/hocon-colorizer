package com.github.ustc_zzzz.hocon

import fastparse.all._

/**
  * @author ustc_zzzz
  */
object HoconTokens {

  import com.github.ustc_zzzz.hocon.HoconObjects._

  // token parts

  private val digits: Parser[_] = P(CharsWhileIn(strings = '0' to '9'))

  private val numeric: Parser[_] = P("0" | (CharIn(strings = '1' to '9') ~ digits.?))

  private val exponent: Parser[_] = P(CharIn(strings = "eE") ~ CharIn(strings = "+-").? ~ digits)

  private val unicode: Parser[_] = P("u" ~ CharIn(strings = '0' to '9', 'a' to 'f', 'A' to 'F').rep(exactly = 4))

  private val commentPart: Parser[_] = P(CharsWhile(_ != '\n', min = 0))

  private val spacesPart: Parser[_] = P(CharsWhile {
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

  private val urlInclusion: Parser[_] = P {
    "url(" ~/ spacesPart.? ~ "\"" ~ quotedChar.rep ~ "\"" ~ spacesPart.? ~ ")"
  }
  private val fileInclusion: Parser[_] = P {
    "file(" ~/ spacesPart.? ~ "\"" ~ quotedChar.rep ~ "\"" ~ spacesPart.? ~ ")"
  }
  private val classPathInclusion: Parser[_] = P {
    "classpath(" ~/ spacesPart.? ~ "\"" ~ quotedChar.rep ~ "\"" ~ spacesPart.? ~ ")"
  }

  // tokens

  val spaces: Parser[Spaces] = P {
    (Index ~ spacesPart.! ~ Index).map(Spaces.tupled).opaque("Spaces")
  }
  val comment: Parser[Comment] = P {
    (("//" | "#").! ~ Index ~ spacesPart.?.! ~ Index ~ commentPart.!).map(Comment.tupled).opaque("Comment")
  }

  val nullPointerValue: Parser[NullPointer] = P {
    "null".!.map(NullPointer).opaque("Null")
  }
  val boolean: Parser[Boolean] = P {
    ("true" | "false" | "yes" | "no" | "on" | "off").!.map(Boolean).opaque("Boolean")
  }
  val number: Parser[Number] = P {
    ("-".? ~ numeric ~ P("." ~ digits).? ~ exponent.?).!.map(Number).opaque("Number")
  }
  val timeUnit: Parser[TimeUnit] = P {
    (numeric ~ spacesPart.? ~ (timeUnitSuffix | timeUnitSuffixAbbr)).!.map(TimeUnit).opaque("TimeUnit")
  }

  val quotedString: Parser[QuotedString] = P {
    ("\"" ~/ quotedChar.rep ~ "\"").!.map(QuotedString).opaque("QuotedString")
  }
  val unquotedString: Parser[UnquotedString] = P {
    (unquotedChar | "/" ~ !"/").rep(min = 1).!.map(UnquotedString).opaque("UnquotedString")
  }
  val rawString: Parser[MultilineString] = P {
    ("\"\"\"" ~/ rawStringChar.rep ~ "\"".rep(min = 3)).!.map(MultilineString).opaque("MultilineString")
  }

  val fieldPathElement: Parser[PathExpressionPart] = P {
    (quotedString | unquotedString).map(_.value).map(PathExpressionPart).opaque("PathExpression")
  }

  val optionalInclusion: Parser[InclusionBodyPart] = P {
    (urlInclusion | fileInclusion | classPathInclusion | quotedString).!.map(InclusionBodyPart).opaque("Inclusion")
  }
  val requirementInclusion: Parser[InclusionBodyPart] = P {
    ("required(" ~/ spacesPart.? ~ optionalInclusion ~ spacesPart.? ~ ")").!.map(InclusionBodyPart).opaque("Inclusion")
  }
}
