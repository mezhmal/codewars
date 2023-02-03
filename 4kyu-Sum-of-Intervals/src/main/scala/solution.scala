@main def solution(params: String*): Unit =
  println(sumOfIntervals(List((1, 5))))
  println(sumOfIntervals(List((1, 2), (6, 10), (3, 5))))
  println(sumOfIntervals(List((1, 5), (1, 5))))
  println(sumOfIntervals(List((1, 4), (7, 10), (3, 5))))
  println(sumOfIntervals(List((1, 4), (7, 10), (3, 5), (6, 9), (1, 2))))
  println(sumOfIntervals(List((499,500), (288,366), (80,295), (493,496), (-298,474), (-197,45), (-468,41), (354,356), (-341,460)))) // Expected 946, but got 2623


def mergePair(interval1: (Int, Int), interval2: (Int, Int)): List[(Int, Int)] = 
  0 match
    case _ if interval1._1 to interval1._2 contains interval2._1 => List((interval1._1, math.max(interval1._2, interval2._2)))
    case _ if interval2._1 to interval2._2 contains interval1._1 => List((interval2._1, math.max(interval1._2, interval2._2)))
    case _ => List(interval1, interval2)

def mergeIntervals(intervals: List[(Int, Int)]): List[(Int, Int)] =
  var (accumulator, rest) = (intervals.head, List[(Int, Int)]())
  for (i <- intervals.sortBy(-_._2)sortBy(_._1)) {
    val r = mergePair(accumulator, i) match
      case List(x) => (x, rest)
      case List(x, y) => (x, rest :+ y)
      case _ => (accumulator, rest)

    accumulator = r._1
    rest = r._2
  }

  if (rest.isEmpty || intervals.size == rest.size + 1) rest :+ accumulator else mergeIntervals(rest :+ accumulator)

def sumOfIntervals(intervals: List[(Int, Int)]): Int =
  mergeIntervals(intervals).map((start, end) => end - start).sum
