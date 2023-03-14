case class Example(input1: String, input2: String, expected: Boolean)

@main def solution(params: String*): Unit =
  val examples = List(
    Example("rkqodlw", "world", true),
    Example("cedewaraaossoqqyt", "codewars", true),
    Example("katas", "steak", false),
    Example("dsatexdrsrvbcoi", "address", true),
    Example("per", "pepper", false),
  )
  for (example <- examples) {
    val result = scramble(example.input1, example.input2)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }

def scramble(s1: String, s2: String): Boolean = s2.diff(s1).isEmpty
