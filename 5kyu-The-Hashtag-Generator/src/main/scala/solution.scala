@main def solution(params: String*): Unit =
    println(generateHashtag(""))
    println(generateHashtag("     "))
    println(generateHashtag("Hello World"))
    println(generateHashtag("   Hello    World   "))
    println(generateHashtag(" Hello there thanks for trying my Kata"))
    println(generateHashtag("Lorem ipsum dolor sit amet consectetur adipiscing elit Aliquam pretium ac nunc nec convallis Cras at enim a diam efficitur mollis Fusce dolor quam suscipit convallis quam a vehicula eleifend ipsum Suspendisse efficitur consequat elit eu pulvinar libero"))
    println(generateHashtag("rEMOVE uNNEEDED cAPS"))
    println(generateHashtag("acronyms should work a s w"))


def generateHashtag(s: String): String = 
    s.trim.split("\\s+").map(_.toLowerCase.capitalize).mkString match
        case hashtag if 1 to 139 contains hashtag.length => s"#$hashtag"
        case _ => ""
