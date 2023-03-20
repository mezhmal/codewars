import scala.math.pow

case class Example(t: Int, k: Int, ls: List[Int], expected: Int)

@main def solution(params: String*): Unit =
  val examples: List[Example] = List(
    Example(174, 3, List(50, 55, 57, 58, 60), 173),
    Example(163, 3, List(50, 55, 56, 57, 58), 163),
    Example(163, 3, List(50), -1),
    Example(163, 1, List(50), 50),
  )
  for (example <- examples) {
    val result = BestTravel.chooseBestSum(example.t, example.k, example.ls)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }

object BestTravel {

    def chooseBestSum(t: Int, k: Int, ls: List[Int]): Int = {
      ls.combinations(k).map(_.sum).filter(_ <= t).maxOption.getOrElse(-1)
    }
}
