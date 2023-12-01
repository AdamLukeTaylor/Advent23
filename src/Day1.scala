object Day1 {
  def part1(input: String): Unit = {
    println("-----------Part 1---------------")
    //dataToEndsSum(input)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def part2(input: String): Unit = {
    println("-----------Part 2---------------")
    val swapped = input.replace("one", "one1one")
      .replace("two", "two2two")
      .replace("three", "three3three")
      .replace("four", "four4four")
      .replace("five", "five5five")
      .replace("six", "six6six")
      .replace("seven", "seven7seven")
      .replace("eight", "eight8eight")
      .replace("nine", "nine9nine")
    dataToEndsSum(swapped)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  }

  def dataToEndsSum(input: String): Unit = {
    val lines = input.split("\n").toList.map(_.toList.filter(_.isDigit))
    println(lines)
    val rows = lines.map(line => line.head.toString.concat(line.reverse.head.toString))
    println(rows)
    println(rows.map(Integer.parseInt).sum)
  }
}
