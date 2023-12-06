object Day6 {
  case class Race(time: Long, distance: Long) {
    def waysToWin: Long = {
      val speeds = (0L to time ).toList // Math.floor(time /2.0).toLong +1
      val distances = speeds.map(s=> time - s)
//      println(speeds)
//      println(distances)
      val covered = (speeds zip distances).map{case (s,d) => s*d}
//      println(covered)
      covered.count(_ > distance)
    }
  }
  def part1(input: String): Unit = {
    println("-----------Part 1---------------")
    val lines = input.split("\n").toList.map(_.trim.replaceAll(" +", " ").split(" ").toList.tail)
    val races = (lines(0) zip lines(1)).map{ case (t,d) => Race(t.toLong, d.toLong)}
    val waysToWin = races.map(_.waysToWin)
    println(races)
    println(waysToWin)
    println(waysToWin.product)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def part2(input: String): Unit = {
    println("-----------Part 2---------------")
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }
}
