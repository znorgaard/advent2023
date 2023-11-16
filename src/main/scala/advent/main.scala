package advent

@main
def main(day: String): Unit = {
  day match {
    case "1" => days.DayOne.main()
    case "2" => days.DayTwo.main()
    case "3" => days.DayThree.main()
    case "4" => days.DayFour.main()
    case "5" => days.DayFive.main()
    case "6" => days.DaySix.main()
    case "7" => days.DaySeven.main()
    case "8" => days.DayEight.main()
    case "9" => days.DayNine.main()
    case "10" => days.DayTen.main()
    case "11" => days.DayEleven.main()
    case "12" => days.DayTwelve.main()
    case "13" => days.DayThirteen.main()
    case "14" => days.DayFourteen.main()
    case "15" => days.DayFifteen.main()
    case "16" => days.DaySixteen.main()
    case "17" => days.DaySeventeen.main()
    case "18" => days.DayEighteen.main()
    case "19" => days.DayNineteen.main()
    case "20" => days.DayTwenty.main()
    case "21" => days.DayTwentyOne.main()
    case "22" => days.DayTwentyTwo.main()
    case "23" => days.DayTwentyThree.main()
    case "24" => days.DayTwentyFour.main()
    case "25" => days.DayTwentyFive.main()
    case _ => println("Unknown day: " + day)
  }
}
