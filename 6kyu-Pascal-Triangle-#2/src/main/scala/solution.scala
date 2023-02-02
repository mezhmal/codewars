import scala.collection.mutable.ListBuffer


@main def solution(params: String*): Unit =
  println(Kata.pascal(1))
  println(Kata.pascal(2))
  println(Kata.pascal(5))
  println(Kata.pascal(7))


object Kata:

  def pascal(p: Int): List[List[BigInt]] = 
    var prevLine = List[BigInt]()
    List.range(1, p + 1).map { i => 
        val line: List[BigInt] = 
          if (i % 2 == 0) {
            val leftHalf = (1 to i / 2).map { j => if (j == 1) BigInt(j) else prevLine(j-2) + prevLine(j-1) }.toList
            leftHalf ++ leftHalf.reverse
          } else {
            val leftHalf = (1 to i / 2 + 1).map { j => if (j == 1) BigInt(j) else prevLine(j-2) + prevLine(j-1) }.toList
            leftHalf ++ leftHalf.reverse.drop(1)
          }
        if (!line.isEmpty) {
          prevLine = line
        }
        line
      }
