@main def solution(params: String*): Unit =
    println(FindTheOddInt.findOdd(Seq(1,2,2,3,3,3,4,3,3,3,2,2,1)))


object FindTheOddInt:
  def findOdd(xs: Seq[Int]): Int =
    val checkIfOdd: Int => Boolean = (value: Int) => value % 2 != 0
    val groupedVals = xs.groupBy(identity).mapValues(_.size).toSeq
    groupedVals.filter( _._2 % 2 != 0 ).head._1
