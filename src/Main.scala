object Main {
  def main(args: Array[String]): Unit = {
    Day10.part1(getFileContent("10_0", example = true))
    Day10.part1(getFileContent("10_1", example = true))
    Day10.part1(getFileContent("10_2", example = true))
    Day10.part1(getFileContent("10", example = false))

    Day10.part2(getFileContent("10", example = true))
    Day10.part2(getFileContent("10", example = false))
  }

  private def getFileContent(day: String, example: Boolean): String = {
    val file = if (example) s"data/example/example_$day.txt" else s"data/real/real_$day.txt"
//    val file = s"data/test_$day.txt"
    val source = scala.io.Source.fromFile(file)
    val lines =
      try source.mkString
      finally source.close()
//    println("Raw---")
//    println(lines)
//    println("---")
    lines
  }

}
