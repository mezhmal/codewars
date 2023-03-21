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
  val groupedDice = dice.groupBy(identity)
  val (sameDiceCount, sameDiceRank) = groupedDice.map(x => (x._2.size, x._1)).toList.sorted.last
  if (sameDiceCount >= 3) {
    val restDice = dice.filter(_ != sameDiceRank) ::: List.fill(sameDiceCount - 3)(sameDiceRank)
    (if (sameDiceRank == 1) 1000 else sameDiceRank * 100) + score(restDice)
  } else {
    groupedDice.getOrElse(1, List[Int]()).size * 100 + groupedDice.getOrElse(5, List[Int]()).size * 50
  }
