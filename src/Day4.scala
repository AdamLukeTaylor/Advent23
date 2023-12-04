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
    val cards = input.split("\n").toList
    var cardCounts = List.fill(cards.length + 1)(0)

    def addToCount(card: Int, toAdd: Int): Unit = {
      cardCounts = cardCounts.updated(card, cardCounts(card) + toAdd)
    }
    cards.foreach { card =>
      val part1Sep = card.indexOf(":")
      val part2Sep = card.indexOf("|")
      val id = Integer.parseInt(card.substring(5, part1Sep).replace(" ", ""))
      addToCount(id, 1)
      val yours = card.substring(part1Sep + 1, part2Sep).split(" ").toList.flatMap(toInt)
      val winning = card.substring(part2Sep + 1, card.length).split(" ").toList.flatMap(toInt)
//            println("Id  " + id)
      //      println(yours)
      //      println(winning)
      //      println(yours.intersect(winning))
      //      println(toScore(yours.intersect(winning)))
      val matches = yours.intersect(winning)
      for (i <- 1 to matches.length){
//        println(s"Adding ${cardCounts(id)} to ${id + i}")
        addToCount(id + i, cardCounts(id))
      }
    }
    println(cardCounts)
    println(cardCounts.sum)
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
