object Day2 {
  case class Result(game: Int, redMax: Int, blueMax: Int, greenMax: Int, possible: Boolean)

  def part1(input: String): Unit = {
    println("-----------Part 1---------------")
    val lines = input.split("\n")
    val games = lines.map { line =>
      val rounds = line.split(":")(1).split(";").toList
      val game = line.replace("Game ", "").split(":").head.toInt
      val results = rounds.map { round =>
        //      println(round)

        val bits = round.split(",")
        val reds = bits.map(getColourCount(_, "red")).max
        val blues = bits.map(getColourCount(_, "blue")).max
        val greens = bits.map(getColourCount(_, "green")).max
        val doable = reds <= 12 && blues <= 14 && greens <= 13
        val out = Result(game = game, redMax = reds, blueMax = blues, greenMax = greens, possible = doable)
//        println(out)
        out
      }
      results
    }.toList
    val possibleOnes = games.map(game =>
      if (game.forall(_.possible)) game.head.game else 0

    )
    println(possibleOnes)
    println(possibleOnes.sum)

    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def part2(input: String): Unit = {
    println("-----------Part 2---------------")
    val lines = input.split("\n")
    val games = lines.map { line =>
      val rounds = line.split(":")(1).split(";").toList
      val game = line.replace("Game ", "").split(":").head.toInt
      val results = rounds.map { round =>
        //      println(round)

        val bits = round.split(",")
        val reds = bits.map(getColourCount(_, "red")).max
        val blues = bits.map(getColourCount(_, "blue")).max
        val greens = bits.map(getColourCount(_, "green")).max
        val doable = reds <= 12 && blues <= 14 && greens <= 13
        val out = Result(game = game, redMax = reds, blueMax = blues, greenMax = greens, possible = doable)
        //        println(out)
        out
      }
      results
    }.toList

    val powers = games.map {
      list =>
        val r = Math.max(list.map(_.redMax).max, 1)
        val g = Math.max(list.map(_.greenMax).max, 1)
        val b = Math.max(list.map(_.blueMax).max, 1)
        r*g*b
    }
    println(powers)
    println(powers.sum)

    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }


  def getColourCount(input: String, colour: String): Int = {
    if (input.contains(colour)) {
      input.replace(" ", "").replace(colour, "").toInt
    } else 0
  }

}
