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

def scramble(s1: String, s2: String): Boolean = 
  val s1Groups = s1.groupBy(_.self)
  val s2Groups = s2.groupBy(_.self)
  s2Groups.keys.forall(c => s1Groups.contains(c) && s1Groups(c).size >= s2Groups(c).size)
