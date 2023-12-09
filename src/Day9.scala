import scala.annotation.tailrec

object Day9 {
  def part1(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 1---------------")
    val predictions = input.split("\n").toList.toList.map(_.split(" ").toList.map(_.toInt)).map { seq =>
      findDiffs(seq,0)
    }
    println(predictions)
    println(predictions.sum)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    println(s"Ended in ${(System.currentTimeMillis() - start) / 1000.0}S")
  }

  //@tailrec
  def findDiffs(nums: List[Int], accumulator: Int): Int = {
    val diffs = if (nums.length >= 2) {
      nums.sliding(2).map { a => a(1) - a.head }.toList
    } else {
      List.fill(1)(0)
    }
//    println(s"diffs = $diffs")
      diffs match {
      case a:List[Int] if a.sum == 0 => nums.last + accumulator
      case _ =>   findDiffs(diffs, nums.last + accumulator)
    }
  }

  def part2(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 2---------------")
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    println(s"Ended in ${(System.currentTimeMillis() - start) / 1000.0}S")
  }
}
