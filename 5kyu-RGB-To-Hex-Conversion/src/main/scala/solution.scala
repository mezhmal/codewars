@main def solution(params: String*): Unit =
    println(Kata.rgb(0, 0, 0))
    println(Kata.rgb(255, 255, 255))
    println(Kata.rgb(255, 255, 300))
    println(Kata.rgb(148, 0, 211))
    println(Kata.rgb(-20, 275, 125))


object Kata:
    def rgb(r: Int, g: Int, b: Int): String =
        Seq(r, g, b).map(0 max _ min 255).map(x => f"${x}%02X").mkString
