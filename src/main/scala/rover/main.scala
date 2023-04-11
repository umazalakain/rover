package rover

object main:
  import rover.*

  def main(args: Array[String]): Unit =
    val grid = Grid(3, 3)
    import grid.*
    val state = State(Position.wrap(0, 0), Direction.N)
    val target = Position.wrap(2, 2)
    val mountains = Set(Position.wrap(0, 1), Position.wrap(1, 1), Position.wrap(2, 1))
    findShortestProgram(state, target, mountains) match {
      case None => println("No path")
      case Some(instructions) => instructions.foreach(println)
    }
