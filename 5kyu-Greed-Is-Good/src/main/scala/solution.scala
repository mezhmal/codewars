import scala.math.pow

case class Example(dice: List[Int], expected: Int)

@main def solution(params: String*): Unit =
  val examples: List[Example] = List(
    Example(List(2, 3, 4, 6, 2), 0),
    Example(List(1, 1, 1, 3, 3), 1000),
    Example(List(2, 2, 2, 3, 3), 200),
    Example(List(3, 3, 3, 3, 3), 300),
    Example(List(4, 4, 4, 3, 3), 400),
    Example(List(5, 5, 5, 3, 3), 500),
    Example(List(6, 6, 6, 3, 3), 600),
    Example(List(1, 1, 1, 1, 3), 1100),
    Example(List(1, 1, 1, 1, 5), 1150),
    Example(List(2, 4, 4, 5, 4), 450),
    Example(List(3, 4, 5, 3, 3), 350),
    Example(List(1, 5, 1, 3, 4), 250),
  )
  for (example <- examples) {
    val result = score(example.dice)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }

def score(dice: List[Int]): Int = 
  dice.groupBy(identity).map((key, value) => (key, value.size)).map {
    case (1, count) => (count / 3) * 1000 + (count % 3) * 100
    case (5, count) => (count / 3) * 500 + (count % 3) * 50
    case (rank, count) => (count / 3) * rank * 100
  }.sum
