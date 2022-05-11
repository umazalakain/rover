import java.lang.Math.floorMod

object Rover {

  case class Grid(width : Int, height : Int)

  case class Position(x : Int, y : Int)

  sealed trait Heading
  object N extends Heading
  object E extends Heading
  object S extends Heading
  object W extends Heading

  case class RoverState(position : Position, heading : Heading)

  sealed trait Command
  object MoveForward extends Command
  object RotateClockwise extends Command
  object RotateAnticlockwise extends Command


  // Part 1: executing commands

  val forward : Heading => Position => Position = {
    case N => pos => pos.copy(y = pos.y + 1)
    case E => pos => pos.copy(x = pos.x + 1)
    case S => pos => pos.copy(y = pos.y - 1)
    case W => pos => pos.copy(x = pos.x - 1)
  }

  val wrap : Grid => Position => Position =
    grid => position => position.copy(x = floorMod(position.x, grid.width), y = floorMod(position.y, grid.height))

  // FIXME: following two definitions might implemented using valueOf and fromOrdinal in Scala 3 enums
  val clockwise : Heading => Heading = {
    case N => E
    case E => S
    case S => W
    case W => N
  }
  val anticlockwise : Heading => Heading = {
    case N => W
    case E => N
    case S => E
    case W => S
  }

  val execCommand : Grid => RoverState => Command => RoverState = {
    // FIXME: surely there must be a better way of mapping a function over a case class field
    grid => rover => {
      case MoveForward => rover.copy(position = wrap(grid)(forward(rover.heading)(rover.position)))
      case RotateClockwise => rover.copy(heading = clockwise(rover.heading))
      case RotateAnticlockwise => rover.copy(heading = anticlockwise(rover.heading))
    }
  }

  // Part 2: path finding

  type Path = List[Position]

  val neighbours : Grid => Position => Set[Position] =
    grid => p => Set(N, E, S, W).map(h => wrap(grid)(forward(h)(p)))

  // mountains as visited positions
  def dijkstra(grid : Grid, target : Position)(visited : Set[Position], candidates : Map[Position, Path]): Option[Path] = {
    candidates.get(target) match {
      // Shortest path has been found
      case Some(path) => Some(path :+ target)
      case None =>
        // Select the best candidate: the one closest to the source
        candidates.minByOption(e => e._2.length) match {
          // No more nodes to explore, could not find the target
          case None => None
          case Some((sPos, sPath)) =>
            val discovered = neighbours(grid)(sPos).map((_, sPath :+ sPos)).toMap
            dijkstra(grid = grid, target = target)(
              visited = visited + sPos,
              candidates = candidates ++ discovered -- visited) // Careful to remove visited nodes
        }
    }
  }

  def shortestPath(grid : Grid, source : Position, target : Position, obstacles : Set[Position]) : Option[Path] =
    dijkstra(grid, target)(obstacles, Map((source, List())))

  // Part 3

  type Commands = List[Command]

  def diffToHeading(from : Position, to : Position) : Option[Heading] =
    (to.x - from.x, to.y - from.y) match {
      case (0 , 1) => Some(N)
      case (1 , 0) => Some(E)
      case (0 , -1) => Some(S)
      case (-1 , 0) => Some(W)
      case _ => None
    }

  // FIXME: Only rotates clockwise, not necessarily the least command solution
  val headingToRotate : Heading => Heading => List[Command] =
    curr => target => if (curr == target) Nil else RotateClockwise :: headingToRotate(clockwise(curr))(target)

  // FIXME: This must surely be in the stdlib
  def sequence[A] : List[Option[A]] => Option[List[A]] = {
    case Nil => Some(List())
    case None :: xs => None
    case Some(x) :: xs => sequence(xs).map(x :: _)
  }
  // FIXME: Can be implemented as a fold
  val headingsToCommands : Heading => List[Heading] => List[Command] = current => {
    case Nil => Nil
    case h :: hs => headingToRotate(current)(h) ++ List(MoveForward) ++ headingsToCommands(h)(hs)
  }
  val pathToCommands : Heading => Path => Option[Commands] =
    current => p => sequence(p.sliding(2).map(xs => diffToHeading(xs(0), xs(1))).toList).map(headingsToCommands(current))


  // Putting it all together

  val showPosition : Position => String = p => s"x : ${p.x}\t y : ${p.y}"
  val showCommand : Command => String = {
    case MoveForward => "MOVE FORWARD"
    case RotateClockwise => "ROTATE CLOCKWISE"
    case RotateAnticlockwise => "ROTATE ANTICLOCKWISE"
  }

  def main(args : Array[String]) : Unit = {
    val grid = Grid(10, 10)
    val source = Position(0, 0)
    val target = Position(4, 4)
    val mountains = Set(Position(3, 3), Position(3, 4), Position(4, 1))
    shortestPath(grid, source, target, mountains) match {
      case None => println("No path")
      case Some(path) =>
        val commands = pathToCommands(N)(path)
        println("Path\n----")
        path.foreach(p => println(showPosition(p)))
        println()
        println("Commands\n-------")
        commands.foreach(_.foreach(c => println(showCommand(c))))
    }
  }
}
