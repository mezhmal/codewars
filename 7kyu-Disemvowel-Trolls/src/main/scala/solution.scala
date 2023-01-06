@main def solution(params: String*): Unit =
    println(disemvowel(params.head))


def disemvowel(str: String): String =
    val vowels = "AaEeIiOoUu".toSet
    str.filterNot(vowels)
