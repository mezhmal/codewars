@main def solution(params: String*): Unit =
  println(MorseDecoder.decode(".... . -.--   .--- ..- -.. ."))  // "HEY JUDE"


object MorseDecoder {
  val morseCodes = Map(
    "-.." -> "D",
    "." -> "E",
    "...." -> "H",
    ".---" -> "J",
    "..-" -> "U",
    "-.--" -> "Y",
  )

  val wordSeparator = " "
  val morseWordSeparator = "   "
  val morseCharacterSeparator = " "

  def decode(msg: String): String = 
    msg.trim.split(morseWordSeparator)
      .map(_.split(morseCharacterSeparator).map(morseCodes).mkString
      ).mkString(wordSeparator)
}
