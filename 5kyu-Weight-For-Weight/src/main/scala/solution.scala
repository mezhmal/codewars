import scala.math.pow

case class Example(input: String, expected: String)

@main def solution(params: String*): Unit =
  val examples: List[Example] = List(
    Example("103 123 4444 99 2000", "2000 103 123 4444 99"),
    Example("2000 10003 1234000 44444444 9999 11 11 22 123", "11 11 2000 10003 22 123 1234000 44444444 9999"),
    Example("", ""),
    Example("10003 1234000 44444444 9999 2000 123456789", "2000 10003 1234000 44444444 9999 123456789"),
    Example("3 16 9 38 95 1131268 49455 347464 59544965313 496636983114762 85246814996697", "3 16 9 38 95 1131268 49455 347464 59544965313 496636983114762 85246814996697"),
  )
  for (example <- examples) {
    val result = WeightSort.orderWeight(example.input)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }

object WeightSort {

  def orderWeight(str: String): String = 
    str.split(" ")
      .map(weight => (weight.trim, weight.trim.map(_.asDigit).sum))
      .sortWith((a, b) => if (a._2 == b._2) a._1 < b._1 else a._2 < b._2)
      .map(_._1)
      .mkString(" ")
}
