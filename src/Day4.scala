object Day4 {
  def part1(input: String): Unit = {
    println("-----------Part 1---------------")
    val cards = input.split("\n").toList
    val score = cards.map { card =>
      val part1Sep = card.indexOf(":")
      val part2Sep = card.indexOf("|")
      val id = Integer.parseInt(card.substring(5, part1Sep).replace(" ", ""))
      val yours = card.substring(part1Sep + 1, part2Sep).split(" ").toList.flatMap(toInt)
      val winning = card.substring(part2Sep + 1, card.length).split(" ").toList.flatMap(toInt)
//      println(id)
//      println(yours)
//      println(winning)
//      println(yours.intersect(winning))
//      println(toScore(yours.intersect(winning)))
      toScore(yours.intersect(winning))
    }.sum
    println(score)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def part2(input: String): Unit = {
    println("-----------Part 2---------------")

    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def toInt(s: String): Option[Int] = {
    try
      Some(s.toInt)
    catch {
      case e: Exception => None
    }
  }
  def toScore(list: List[Int]): Int =
    Math.pow(2, list.length - 1).toInt

}
