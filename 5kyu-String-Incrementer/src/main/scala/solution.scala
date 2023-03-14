case class Example(input: String, expected: String)

@main def solution(params: String*): Unit =
  val examples = List(
    Example("", "1"),
    Example("1", "2"),
    Example("008", "009"),
    Example("009", "010"),
    Example("foo", "foo1"),
    Example("foobar99", "foobar100"),
    Example("foo99bar0099", "foo99bar0100"),
  )
  for (example <- examples) {
    val result = incrementString(example.input)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }

def incrementString(s: String): String = 
  s.reverse.takeWhile(_.isDigit).reverse match
    case tail if tail.nonEmpty =>
      s.dropRight(tail.size) + s"%0${tail.size}d".format(tail.toInt + 1)
    case _ => s + "1"
