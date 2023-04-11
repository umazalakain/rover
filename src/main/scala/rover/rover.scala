package rover

import cats.instances.list.*
import cats.syntax.all.*
import cats.{Foldable, Show}

import java.lang.Math.floorMod as mod
import scala.annotation.tailrec

enum Direction:
  case N
  case E
  case S
  case W

import Direction.*
export Direction.*

enum Instruction:
  case Forward
  case Left
  case Right

import Instruction.*
export Instruction.*

type Update[A] = A => A
type Program = List[Instruction]

val turnLeft: Update[Direction] =
  case N => W
  case E => N
  case S => E
  case W => S

val turnRight: Update[Direction] =
  case N => E
  case E => S
  case S => W
  case W => N

final case class Grid(width: Int, height: Int):
  case class Position private(x: Int, y: Int)
  object Position:
    def wrap(unwrappedX: Int, unwrappedY: Int) =
      Position(mod(unwrappedX, width), mod(unwrappedY, height))

  given Show[Position] = p => s"x : ${p.x}\t y : ${p.y}"

  type Path = List[Position]

  final case class State(pos: Position, dir: Direction)

  def update(fx: Update[Int], fy: Update[Int]): Update[Position] =
    p => Position.wrap(fx(p.x), fy(p.y))

  def binary(f: (Int, Int) => Int)(left: Position, right: Position): Position =
    Position.wrap(f(left.x, right.x), f(left.y, right.y))

  def neighbours(pos: Position): List[Position] =
    val steps = List(
      Position.wrap(0, 1),
      Position.wrap(1, 0),
      Position.wrap(0, -1),
      Position.wrap(-1, 0)
    )
    steps.map(binary(_ + _)(pos, _))

  val forward: Direction => Update[Position] =
    case N => update(identity, _ + 1)
    case S => update(identity, _ - 1)
    case E => update(_ + 1, identity)
    case W => update(_ - 1, identity)

  val eval: Instruction => Update[State] =
    case Forward => s => s.copy(pos = forward(s.dir)(s.pos))
    case Left => s => s.copy(dir = turnLeft(s.dir))
    case Right => s => s.copy(dir = turnRight(s.dir))

  val evals: Program => Update[State] =
    case Nil => identity
    case x :: xs => eval(x) andThen evals(xs)

  @tailrec
  def dijkstra(target: Position, ignore: Set[Position], explore: Map[Position, Path]): Option[Path] =
    // Check if we already know how to get there
    explore.get(target) match {
      case Some(path) => Some(path :+ target)
      case None =>
        // Select the least unexplored option first
        explore.minByOption(_._2.length) match {
          case None => None
          case Some((candidate, path)) =>
            val nextIgnore = ignore + candidate
            val nextCandidates = neighbours(candidate).map(_ -> (path :+ candidate))
            val nextExplore = (explore ++ nextCandidates).filterNot(x => nextIgnore.contains(x._1))
            dijkstra(target, nextIgnore, nextExplore)
        }
    }

  def findStep(state: State, target: Position): Option[Program] =
    val candidates = List(List.empty, List(Forward), List(Right, Forward), List(Left, Forward), List(Right, Right, Forward))
    candidates.find(p => evals(p)(state).pos == target)

  def findProgram(state: State): Path => Option[Program] =
    case Nil => Some(List.empty)
    case p :: ps =>
      for
        program <- findStep(state, p)
        continuation <- findProgram(evals(program)(state))(ps)
      yield program ++ continuation

  def findShortestProgram(state: State, target: Position, avoid: Set[Position]): Option[Program] =
    for
      path <- dijkstra(target, avoid, Map(state.pos -> List.empty))
      program <- findProgram(state)(path)
    yield program
