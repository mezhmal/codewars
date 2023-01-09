@main def solution(params: String*): Unit =
    val result1 = DirReduction.dirReduc(Array("NORTH", "SOUTH", "SOUTH", "EAST", "WEST", "NORTH", "WEST"))
    println(s"${result1.length}: ${result1.mkString(" ")}")
    val result2 = DirReduction.dirReduc(Array("NORTH", "WEST", "SOUTH", "EAST"))
    println(s"${result2.length}: ${result2.mkString(" ")}")
    val result3 = DirReduction.dirReduc(Array("NORTH", "SOUTH", "SOUTH", "EAST", "WEST", "NORTH", "NORTH"))
    println(s"${result3.length}: ${result3.mkString(" ")}")


object DirReduction:
    def dirReduc(arr: Array[String]): Array[String] =
        var complete = false
        var a = arr.mkString(" ") + " "
        val pattern = "(NORTH SOUTH |SOUTH NORTH |EAST WEST |WEST EAST )"
        while (!complete) {
            val directionsSizeBeforeReduce = a.length
            a = a.replaceAll(pattern, "")
            val directionsSizeAfterReduce = a.length
            complete = directionsSizeBeforeReduce == directionsSizeAfterReduce
        }
        a.split(" ")
 