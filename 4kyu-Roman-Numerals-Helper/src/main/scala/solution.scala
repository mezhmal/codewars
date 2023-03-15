import scala.math.pow

case class ExampleFrom(roman: String, expected: Int)
case class ExampleTo(number: Int, expected: String)

@main def solution(params: String*): Unit =
  val myRoman = RomanNumerals()
  val examplesFrom: List[ExampleFrom] = List(
    ExampleFrom("MM", 2000),
    ExampleFrom("MDCLXVI", 1666),
    ExampleFrom("M", 1000),
    ExampleFrom("CD", 400),
    ExampleFrom("XC", 90),
    ExampleFrom("XL", 40),
    ExampleFrom("XXX", 30),
    ExampleFrom("IV", 4),
    ExampleFrom("I", 1),
  )
  for (example <- examplesFrom) {
    val result = myRoman.fromRoman(example.roman)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }
  val examplesTo: List[ExampleTo] = List(
    ExampleTo(2000, "MM"),
    ExampleTo(1999, "MCMXCIX"),
    ExampleTo(1666, "MDCLXVI"),
    ExampleTo(1000, "M"),
    ExampleTo(400, "CD"),
    ExampleTo(90, "XC"),
    ExampleTo(40, "XL"),
    ExampleTo(30, "XXX"),
    ExampleTo(9, "IX"),
    ExampleTo(8, "VIII"),
    ExampleTo(4, "IV"),
    ExampleTo(1, "I"),
  )
  for (example <- examplesTo) {
    val result = myRoman.toRoman(example.number)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }

class RomanNumerals {
  val mapRomanToInt = Map(
    "I" -> 1,
    "V" -> 5,
    "X" -> 10,
    "L" -> 50,
    "C" -> 100,
    "D" -> 500,
    "M" -> 1000
  )
  val mapIntToRoman = Map(
    1 -> "I",
    4 -> "IV",
    5 -> "V",
    9 -> "IX",
    10 -> "X",
    40 -> "XL",
    50 -> "L",
    90 -> "XC",
    100 -> "C",
    400 -> "CD",
    500 -> "D",
    900 -> "CM",
    1000 -> "M",
  )

  def getClosestRankForRoman(number: Int): Option[Int] =
    val keysLowerThan = mapIntToRoman.keys.toList.sorted.filter(_ < number)
    if (keysLowerThan.nonEmpty) Some(keysLowerThan.last) else None

  def fromRoman(roman: String): Int = 
    val values = roman.split("").map(mapRomanToInt).toList
    (values zip values.tail :+ 0).map((current, next) => if (current < next) current * -1 else current).sum
  
  def toRoman(number: Int): String =
    if (mapIntToRoman.contains(number)) {
      mapIntToRoman(number)
    } else {
      getClosestRankForRoman(number) match
        case Some(rank) => 
          val count = number / rank
          mapIntToRoman(rank) * count + toRoman(number - rank * count)
        case None => ""
    }
}
