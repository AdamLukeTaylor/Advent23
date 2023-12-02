object Main {
  def main(args: Array[String]): Unit = {
    Day2.part1(getFileContent("2", example = true))
    Day2.part1(getFileContent("2", example = false))
    Day2.part2(getFileContent("2", example = true))
    Day2.part2(getFileContent("2", example = false))
  }

  private def getFileContent(day: String, example: Boolean): String = {
    val file = if (example) s"data/example_$day.txt" else s"data/real_$day.txt"

    val source = scala.io.Source.fromFile(file)
    val lines = try source.mkString finally source.close()
//    println("Raw---")
//    println(lines)
//    println("---")
    lines
  }

}