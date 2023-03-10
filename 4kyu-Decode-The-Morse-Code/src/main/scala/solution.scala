@main def solution(params: String*): Unit =
  val examples = List(
    Map(
      "data" -> "00000011001100110011000000110000001111110011001111110011111100000000000000110011111100111111001111110000001100110011111100000011111100110011000000110",
      "shouldBe" -> "HEY JUDE"
    ),
    Map(
      "data" -> "00000011001100110011000000110000001111110011001111110011111100000000000000110011111100111111001111110000001100110011111100000011111100110011000000110000",
      "shouldBe" -> "HEY JUDE"
    ),
    Map(
      "data" -> "110011",
      "shouldBe" -> "I"
    ),
    Map(
      "data" -> "11111100111111",
      "shouldBe" -> "M"
    ),
    Map(
      "data" -> "1110111",
      "shouldBe" -> "M"
    ),
    Map(
      "data" -> "111",
      "shouldBe" -> "E"
    ),
  )
  for (example <- examples) {
    val result = MorseDecoder.decodeMorse(MorseDecoder.decodeBits(example("data")))
    println(s"$result <- ${example("shouldBe")}")
  }


object MorseDecoder {
  val morseCodes = Map(
    ".-" -> "A",
    "-.." -> "D",
    "." -> "E",
    "...." -> "H",
    ".." -> "I",
    ".---" -> "J",
    "--" -> "M",
    "-" -> "T",
    "..-" -> "U",
    "-.--" -> "Y",
  )

  val wordSeparator = " "
  val morseDot = "."
  val morseDash = "-"
  val morseWordSeparator = "   "
  val morseCharacterSeparator = " "

  def decodeBits(bits: String): String = {
    var characterBits = List[String]()
    var bitsTail = bits.dropWhile(_ == '0').reverse.dropWhile(_ == '0').reverse
    while (!bitsTail.isEmpty()) {
      characterBits = characterBits :+ bitsTail.takeWhile(_ == bitsTail.head)
      bitsTail = bitsTail.dropWhile(_ == bitsTail.head)
    }
    val bitsInCharacter = characterBits.map(_.size).min
    characterBits.map(_ match {
      case x if x.head == '0' => x.size match {
        case s if s > bitsInCharacter * 3 => morseWordSeparator
        case s if s > bitsInCharacter => morseCharacterSeparator
        case _ => ""
      }
      case x if x.head == '1' => if (x.size > bitsInCharacter) morseDash else morseDot
      case x => x.head.toString
    }).mkString
  }

  def decodeMorse(morseCode: String): String = {
    morseCode.trim.split(morseWordSeparator)
      .map(_.split(morseCharacterSeparator).map(morseCodes).mkString)
      .mkString(wordSeparator)
  }
}
