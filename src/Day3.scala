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


    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

}
