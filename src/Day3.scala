object Day3 {
  def part1(input: String): Unit = {
    println("-----------Part 1---------------")
    val lines = input.split("\n").toList.sliding(3)
    val nonAdj = lines.map { group =>
      val regex = """[%@=/\-#$*\\+]""".r

      val first = group(0)
      val second = group(1)
      val third = group(2)
      val above = regex.findAllMatchIn(first).map(_.start).toList
      val below = regex.findAllMatchIn(third).map(_.start).toList
      val merged = second.zipWithIndex.map {
        case (elem, idx) => if (above.contains(idx) || below.contains(idx)) {
          '*'
        } else elem

      }
      //      println(merged)
      val bits = merged.toList.mkString.split("\\.")
      bits.map(toInt).toList.filter(_.nonEmpty).map(_.get)
    }.toList.flatten
    val all = input
      .replaceAll("\\*", ".")
      .replaceAll("\n", ".")
      .split("\\.").toList.map(toInt).filter(_.nonEmpty).map(_.get)
    println(all)
    println(all.sum)
    println(nonAdj)
    println(nonAdj.sum)
    println(all.sum - nonAdj.sum)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def part2(input: String): Unit = {
    println("-----------Part 2---------------")
    val lines = input.split("\n").toList.sliding(3).toList
    val gear = lines.map { group =>
      val regexGear = """\*""".r
      val regexTwoNum = """\d\.\d""".r
      val regexOneNum = """\d""".r
      val regexNoNum = """\.\.\.""".r
      val first = group(0)
      val second = group(1)
      val third = group(2)
      println(second)
      val anyGears = regexGear.findAllMatchIn(second).map(_.start).toList
      anyGears.map { gear =>
        val above = first.substring(Math.max(0, gear - 1), Math.min(gear + 2, first.length))
        val here = second.substring(Math.max(0, gear - 1), Math.min(gear + 2, second.length))
        val below = third.substring(Math.max(0, gear - 1), Math.min(gear + 2, third.length))
        println("Gears " + above + "  " + below)
        val (up2, here2, below2) = (regexTwoNum.findAllMatchIn(above).map(_.start).toList, regexTwoNum.findAllMatchIn(here).map(_.start).toList, regexTwoNum.findAllMatchIn(below).map(_.start).toList)
        val (up1, here1, below1) = (regexOneNum.findAllMatchIn(above).map(_.start).toList, regexOneNum.findAllMatchIn(here).map(_.start).toList, regexOneNum.findAllMatchIn(below).map(_.start).toList)
        val (up0, here0, below0) = (regexNoNum.findAllMatchIn(above).map(_.start).toList, regexNoNum.findAllMatchIn(here).map(_.start).toList, regexNoNum.findAllMatchIn(below).map(_.start).toList)
        println(up2)
        if (up2.nonEmpty) {
          //on same row
          val x = List(getNumberAtIndex(gear-1,first), getNumberAtIndex(gear+1, first))
          println("xxx" + x)
        } else if ( here2.nonEmpty) {
          //on same row

        } else if ( below2.nonEmpty) {
          //on same row

        }
        else {
          // across 2
        }
      }

    }

    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def getNumberAtIndex(index: Int, where: String): Int = {
    var rightRunner = 0
    var leftRunner = 0
    while ((rightRunner + index) <= where.length && where(rightRunner + index).isDigit) {
      rightRunner = rightRunner + 1
    }
    rightRunner = rightRunner - 1
    while ((leftRunner + index) >=0 && where(leftRunner + index).isDigit) {
      leftRunner = leftRunner - 1
    }
    leftRunner = leftRunner + 1
    println( where)
    println( index + leftRunner)
    println( index + rightRunner)
    where.substring(index + leftRunner, index + rightRunner+1).toInt
  }

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

}
