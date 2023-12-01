object Main {
  def main(args: Array[String]): Unit = {
    Day1.part1(getFileContent("1", example = true))
    Day1.part1(getFileContent("1", example = false))
    Day1.part2(getFileContent("1", example = true))
    Day1.part2(getFileContent("1", example = false))
  }

  private def getFileContent(day: String, example: Boolean): String = {
    val file = if (example) s"data/example_$day.txt" else s"data/real_$day.txt"

    val source = scala.io.Source.fromFile(file)
    val lines = try source.mkString finally source.close()
    println("Raw---")
    println(lines)
    println("---")
    lines
  }

}