package advent

@main
def main(day: String): Unit = {
  day match {
    case "1" => days.DayOne.main()
    case _ => println("Unknown day: " + day)
  }
}
