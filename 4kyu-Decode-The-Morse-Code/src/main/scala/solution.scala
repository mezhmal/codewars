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

  val morseDot = "."
  val morseDash = "-"
  val morseWordSeparator = "   "
  val morseCharacterSeparator = " "

  def decodeBits(bits: String): String = {
    var binaryCharacters = List[String]()
    var bitsTail = bits.dropWhile(_ == '0').reverse.dropWhile(_ == '0').reverse
    while (!bitsTail.isEmpty()) {
      binaryCharacters = binaryCharacters :+ bitsTail.takeWhile(_ == bitsTail.head)
      bitsTail = bitsTail.dropWhile(_ == bitsTail.head)
    }
    val bitsInUnit = binaryCharacters.map(_.size).min
    binaryCharacters.map(binaryCharacter => {
      (binaryCharacter.head, binaryCharacter.size/bitsInUnit) match {
        case ('1', 1) => morseDot
        case ('1', 3) => morseDash
        case ('0', 1) => ""
        case ('0', 7) => morseWordSeparator
        case ('0', 3) => morseCharacterSeparator
        case _ => "?"
      }
    }).mkString
  }

  def decodeMorse(morseCode: String): String = {
    morseCode.trim.split(morseWordSeparator)
      .map(_.split(morseCharacterSeparator).map(morseCodes).mkString)
      .mkString(" ")
  }
}
