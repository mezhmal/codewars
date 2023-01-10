import scala.collection.mutable.ListBuffer


@main def solution(params: String*): Unit =
    println(Snail.snail(List(
                            List(1, 2, 3),
                            List(4, 5, 6),
                            List(7, 8, 9)
                        )))
    println(Snail.snail(List(
                            List(1, 2, 3),
                            List(8, 9, 4),
                            List(7, 6, 5)
                        )))
    println(Snail.snail(List(
                            List(1, 2),
                            List(4, 3),
                        )))
    println(Snail.snail(List(
                            List(1),
                        )))
    println(Snail.snail(List(List())))


object Snail:
    def snail(xs: List[List[Int]]): List[Int] = 
        var i1 = 0
        var i2 = xs(0).size - 1
        var j1 = 0
        var j2 = xs(0).size - 1
        val result: ListBuffer[Int] = ListBuffer()

        while ((i1 < i2)) {
            // →
            for (j <- j1 to j2) result += xs(i1)(j)
            i1 += 1
            // ↓
            for (i <- i1 to i2) result += xs(i)(j2)
            j2 -= 1
            // ←
            for (j <- j2 to j1 by -1) result += xs(i2)(j)
            i2 -= 1
            // ↑
            for (i <- i2 to i1 by -1) result += xs(i)(j1)
            j1 += 1
        }

        // in order to get element in center of the array
        if (i1 == i2) {
            result += xs(i1)(j1)
        }

        result.toList
