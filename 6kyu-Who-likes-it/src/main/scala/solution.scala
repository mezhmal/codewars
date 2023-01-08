@main def solution(params: String*): Unit =
    println(likes(Array()))
    println(likes(Array("Peter")))
    println(likes(Array("Jacob", "Alex")))
    println(likes(Array("Max", "John", "Mark")))
    println(likes(Array("Alex", "Jacob", "Mark", "Max")))


def likes(names: Array[String]): String =
    names match 
      case Array() => "no one likes this"
      case Array(x) => s"$x likes this"
      case Array(x, y) => s"$x and $y like this"
      case Array(x, y, z) => s"$x, $y and $z like this"
      case Array(x, y, rest*) => s"$x, $y and ${rest.length} others like this"
      case _ => "Implement me <3"
