object Day7 {
  case class Card(name: Char, strength: Char)
  case class Hand(cards: List[Card]) extends Ordered[Hand] {
    override def compare(that: Hand): Int = {
      if (handTypeWithJoker > that.handTypeWithJoker) {
        1
      } else if (handTypeWithJoker < that.handTypeWithJoker) {
        -1
      } else -cards.map(_.strength).mkString.compareTo(that.cards.map(_.strength).mkString)
    }

    def handTypeWithJoker: Int = {
      val nonJokers = cards.filter(_.name!= 'J').groupBy(identity).view.mapValues(_.size)
      val bestForJoker = if(nonJokers.nonEmpty) nonJokers.maxBy(_._2)._1 else Card('A', 'A')
      val subbed = cards.map {
        case Card('J', _) => bestForJoker
        case card         => card
      }
//      println(s"orig: ${cards.map(_.name).mkString}")
      handType(Some(subbed))
    }
    def handType(maybe: Option[List[Card]] = None): Int = {
      val cardsToUse = maybe.getOrElse(cards)
//      println(s"new: ${cardsToUse.map(_.name).mkString}")
      val occurs = cardsToUse.groupBy(identity).view.mapValues(_.size)
      if (occurs.size == 1) 7
      else if (occurs.values.max == 4) 6
      else if (occurs.values.max == 3 && occurs.values.min == 2) 5
      else if (occurs.values.max == 3 && occurs.values.min == 1) 4
      else if (occurs.values.toList.count(_ == 2) == 2) 3
      else if (occurs.values.toList.count(_ == 2) == 1) 2
      else 1
    }
  }
  case class Play(hand: Hand, bid: Long) extends Ordered[Play] {
    override def compare(that: Play): Int = hand.compareTo(that.hand)
  }
  def part1(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 1---------------")
    val plays = input.split("\n").toList.map(_.split(" ").toList).map { row =>
      Play(parseCards(row.head), row(1).toLong)
    }.sorted
    val ranks = plays.zipWithIndex.map {
      case (play, index) => play.bid * (index + 1)
    }
    println(plays.map(play => s"${play.hand.cards.map(_.name).mkString} - ${play.hand.handType()}"))
    println(ranks)
    println(ranks.sum)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    println(s"Ended in ${(System.currentTimeMillis() - start) / 1000.0}S")
//    248721426 low
  }

  def part2(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 2---------------")
    val plays = input.split("\n").toList.map(_.split(" ").toList).map { row =>
      Play(parseCards(row.head), row(1).toLong)
    }.sorted
    val ranks = plays.zipWithIndex.map {
      case (play, index) => play.bid * (index + 1)
    }
    println(plays.map(play => s"${play.hand.cards.map(_.name).mkString} - ${play.hand.handTypeWithJoker}"))
    println(ranks)
    println(ranks.sum)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
//    250019693 too low
    println(s"Ended in ${(System.currentTimeMillis() - start) / 1000.0}S")
  }

  def parseCards(input: String): Hand =
    Hand(input.toCharArray.toList.map(parseCard))
  def parseCard(card: Char): Card = card match {
    case 'A' => Card(card, 'A')
    case 'K' => Card(card, 'B')
    case 'Q' => Card(card, 'C')
    case 'T' => Card(card, 'E')
    case '9' => Card(card, 'F')
    case '8' => Card(card, 'G')
    case '7' => Card(card, 'H')
    case '6' => Card(card, 'I')
    case '5' => Card(card, 'J')
    case '4' => Card(card, 'K')
    case '3' => Card(card, 'L')
    case '2' => Card(card, 'M')
    case 'J' => Card(card, 'N')
  }
}
