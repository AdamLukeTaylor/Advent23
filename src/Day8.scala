object Day8 {
  case class Directions(in: String) {
    def get(index: Int) = in.charAt(index % in.length)
  }

  case class Node(in: String) {
    def name: String = in.split(" = ").head

    def left: String = in.substring(in.indexOf("(") + 1, in.indexOf(","))

    def right: String = in.substring(in.indexOf(", ") + 2, in.indexOf(")"))
  }

  def part1(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 1---------------")
    val lines = input.split("\n").toList
    val directions = Directions(lines.head)
    val nodes = lines.zipWithIndex.filter(_._2 >= 2).map(line => Node(line._1))
    println(nodes.map(node => node.left + "  " + node.right))
    var currentNode = nodes.find(_.name == "AAA").get
    var index = 0
    while (currentNode.name != "ZZZ") {
      val dir = directions.get(index)
      if (dir == 'L') {
        currentNode = nodes.find(_.name == currentNode.left).get
      } else currentNode = nodes.find(_.name == currentNode.right).get
      println(s"$dir to ${currentNode.name}")
      index += 1
    }
    println(s"Took $index")
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    println(s"Ended in ${(System.currentTimeMillis() - start) / 1000.0}S")
  }

  def part2(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 2---------------")
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    println(s"Ended in ${(System.currentTimeMillis() - start) / 1000.0}S")
  }
}
