@main def solution(params: String*): Unit =
    println(HumanTime.formatDuration(0))
    println(HumanTime.formatDuration(1))
    println(HumanTime.formatDuration(62))
    println(HumanTime.formatDuration(200))
    println(HumanTime.formatDuration(3662))
    println(HumanTime.formatDuration(2 * 365 * 24 * 60 * 60))
    

object HumanTime:

    def formatDuration(seconds: Int): String =
        val secondsInMinute = 60
        val secondsInHour = 60 * secondsInMinute
        val secondsInDay = 24 * secondsInHour
        val secondsInYear = 365 * secondsInDay
        List(
            ("year", seconds / secondsInYear),
            ("day", seconds % secondsInYear / secondsInDay),
            ("hour", seconds % secondsInDay / secondsInHour),
            ("minute", seconds % secondsInHour / secondsInMinute),
            ("second", seconds % secondsInMinute)
        ).collect {
            case (unit, 1) => s"1 ${unit}"
            case (unit, value) if value > 1 => s"$value ${unit}s"
        } match {
            case List() => "now"
            case List(single) => single
            case xs => s"${xs.init.mkString(", ")} and ${xs.last}"
        }
