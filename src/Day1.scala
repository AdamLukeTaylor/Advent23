object Day1 {
  def part1(input: String): Unit = {
    println("-----------Part 1---------------")
    val lines = input.split("\n").toList.map(_.toList.filter(_.isDigit))
    println(lines)
    val rows = lines.map(line => line.head.toString.concat(line.reverse.head.toString))
    println(rows)
    println(rows.map(Integer.parseInt).sum)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def part2(input: String): Unit = {
    println("-----------Part 2---------------")

    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }
}
