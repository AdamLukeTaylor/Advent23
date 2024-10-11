object Day10 {
  case class Coord(x: Int, y: Int) {
    def up: Coord = Coord(x, y - 1)

    def down: Coord = Coord(x, y + 1)

    def left: Coord = Coord(x - 1, y)

    def right: Coord = Coord(x + 1, y)
  }

  case class Cell(input: Char, coord: Coord, step: Option[Int]) {
    def connectsTo: List[Coord] = input match {
      case '|' => List(coord.up, coord.down)
      case '-' => List(coord.right, coord.left)
      case 'L' => List(coord.up, coord.right)
      case 'J' => List(coord.up, coord.left)
      case '7' => List(coord.down, coord.left)
      case 'F' => List(coord.down, coord.right)
      case '.' | 'S' => List.empty
    }
    def neighbours: List[Coord] = List(coord.up, coord.down, coord.right, coord.left)
  }
object Maze {
  def createMaze(input: String): Maze = {
    def rows: List[String] = input.split("\n").toList

    def cells(row: Int): List[Char] = rows(row).toCharArray.toList

    def maze: List[List[Cell]] = rows.zipWithIndex.map { case (row, i) => row.toList.zipWithIndex.map { case (col, j) => Cell(col, Coord(i, j), None) } }
    Maze(maze)
  }
}
  case class Maze(maze: List[List[Cell]]) {

    def mark (cell: Cell, step: Int): Option[Maze] = {
      if(maze(cell.coord.x)(cell.coord.y).step.isEmpty) {
        val item = maze(cell.coord.x)(cell.coord.y).copy(step = Some(step))
        Some(Maze(maze.updated(cell.coord.x, maze(cell.coord.x).updated(cell.coord.y,item))))
      }else None
    }


    def get(coord: Coord): Option[Cell] = if (coord.x >= 0 && coord.x < numRows && coord.y >= 0 && coord.y < numCols) {
      Some(maze(coord.x)(coord.y))
    } else None

    val numRows: Int = maze.length

    val numCols: Int = maze.head.length

    val start: Cell = maze.flatMap { row =>
      row.find(cell => cell.input == 'S')
    }.head
    def getStartCells: List[Cell] = {
      start.neighbours.flatMap(coord => get(coord))
    }

  }

  def part1(input: String): Unit = {
    val start = System.currentTimeMillis()
    println("-----------Part 1---------------")
    val maze = Maze.createMaze(input)
    println(maze.getStartCells)
    maze.getStartCells.map{cell =>
      var copy = maze
      var current: Option[Cell] = Some(cell)
      var step = 0
      while (current.isDefined&& current.get.connectsTo.nonEmpty && copy.mark(current.get, step).nonEmpty){
        copy = copy.mark(current.get, step).get
        step +=1
        val next =current.get.connectsTo.filter(coord => copy.get(coord).isDefined && copy.get(coord).get.step.isDefined)
        current= copy.get(next.head)
      }


    }
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
