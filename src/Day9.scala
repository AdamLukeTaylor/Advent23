import scala.annotation.tailrec

object Day9 {
  def part1(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 1---------------")
    val predictions = input.split("\n").toList.toList.map(_.split(" ").toList.map(_.toInt)).map { seq =>
      findDiffs(seq,List.empty,choseLast)
    }
    println(predictions)
//    println(predictions.sum)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    println(s"Ended in ${(System.currentTimeMillis() - start) / 1000.0}S")
  }
  private def choseLast(list: List[Int]): Int = list.last
  private def choseFirst(list: List[Int]): Int = list.head

  //@tailrec
  def findDiffs(nums: List[Int], accumulator: List[Int], chooser: (List[Int]=> Int)): List[Int] = {
    val diffs = if (nums.length >= 2) {
      nums.sliding(2).map { a => a(1) - a.head }.toList
    } else {
      List.fill(1)(0)
    }
//    println(s"$nums $diffs $accumulator")
      diffs match {
      case a:List[Int] if a.sum == 0 =>
//        println(s"${chooser(nums)} + $accumulator")
        accumulator.appended(chooser(nums))
      case _ =>
//        println(s"loop ${chooser(nums)} - $accumulator")
        findDiffs(diffs, accumulator.appended(chooser(nums)) , chooser)
    }
  }

//  2 - 0 = 2
//  0 - 2 = -2
//  3 - -2 = 5
//  10 - 5 = 5

  def part2(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 2---------------")
    val predictions = input.split("\n").toList.toList.map(_.split(" ").toList.map(_.toInt)).map { seq =>
      val elms = findDiffs(seq, List.empty, choseFirst).appended(0).reverse
      println(elms)
      val out = elms.foldLeft(0)((x, y) => y -x)
      println(out)
      out
    }
    println("Summed" + predictions.sum)
//    println(s"summed = ${predictions.sum}")
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    println(s"Ended in ${(System.currentTimeMillis() - start) / 1000.0}S")
  }
}
