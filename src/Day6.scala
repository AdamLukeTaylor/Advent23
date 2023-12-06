object Day6 {
  case class Race(time: Long, distance: Long) {
    def waysToWin: Long = {
      val speeds = (0L to time).toList // Math.floor(time /2.0).toLong +1
      val distances = speeds.map(s => time - s)
//      println(speeds)
//      println(distances)
      var covered = 0L
      var index = 0
      val step = 1
      // count pre cross
      for (index <- 7051000L until 7052000 by step) {
//        println(
//          s"speed ${speeds(index.toInt)} distance ${distance} both ${speeds(index.toInt) * distances(index.toInt)} a ${index.toInt}"
//        )
        if (speeds(index.toInt) * distances(index.toInt) < distance) {
//          println("loser")
        } else {
//          println("winner")
          covered += step
        }
        if (index % 100000 == 0) println(s"index = ${index}")
      }
      println("--------------------cross")
      // count post cross

      covered += time -(7052000*2)

      // count cross back
      println("--------------------cross back")
      for (index <- time-7052000 to time - 7051000 by step) {
//        println(
//          s"speed ${speeds(index.toInt)} distance ${distance} both ${speeds(index.toInt) * distances(index.toInt)} a ${index.toInt}"
//        )
        if (speeds(index.toInt) * distances(index.toInt) < distance) {
//          println("loser")
        } else {
//          println("winner")
          covered += step
        }
        if (index % 100000 == 0) println(s"index = ${index}")
      }

      println(index)
      covered
    }
  }
  def part1(input: String): Unit = {
    println("-----------Part 1---------------")
    val lines = input.split("\n").toList.map(_.trim.replaceAll(" +", " ").split(" ").toList.tail)
    val races = (lines(0) zip lines(1)).map { case (t, d) => Race(t.toLong, d.toLong) }
    val waysToWin = races.map(_.waysToWin)
    println(races)
    println(waysToWin)
    println(waysToWin.product)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def part2(input: String): Unit = {
    println("-----------Part 2---------------")
    val lines = input.split("\n").toList.map(_.trim.replaceAll(" +", "").split(":").toList.tail)
    val races = (lines(0) zip lines(1)).map { case (t, d) => Race(t.toLong, d.toLong) }
    val waysToWin = races.map(_.waysToWin)
    println(races)
    println(waysToWin)
    println(waysToWin.product)
    // 27561422  too low
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }
}
