package rover

import Math.abs
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Test}

object rover extends Properties("rover"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    super.overrideParameters(p.withMinSize(1).withMaxSize(5))

  given Arbitrary[Instruction] = Arbitrary(Gen.oneOf[Instruction](Forward, Left, Right))
  given Arbitrary[Direction] = Arbitrary(Gen.oneOf(N, E, S, W))
  given Arbitrary[Grid] = Arbitrary(
    for
      size <- Gen.size
      height <- Gen.choose(1, size + 1)
      width <- Gen.choose(1, size + 1)
    yield Grid(width, height)
  )
  implicit def arbitrary_Position(g: Grid): Arbitrary[g.Position] = Arbitrary(
    for
      x <- Gen.choose(0, g.width - 1)
      y <- Gen.choose(0, g.height - 1)
    yield g.Position.wrap(x, y)
  )
  implicit def arbitrary_State(g: Grid): Arbitrary[g.State] =
    given Arbitrary[g.Position] = arbitrary_Position(g)
    Arbitrary(
      for
        pos <- Arbitrary.arbitrary[g.Position]
        dir <- Arbitrary.arbitrary[Direction]
      yield g.State(pos, dir)
    )

  property("Given a valid path, one can find a list of instructions that if followed will take you to its end") =
    forAll { (grid: Grid) =>
      import grid.*
      given Arbitrary[State] = arbitrary_State(grid)
      forAll { (state: State, instructions: List[Instruction]) =>
        val validPath = instructions.scanLeft(state)((st, instr) => eval(instr)(st)).map(_.pos)
        val foundInstructions = findProgram(state)(validPath)
        foundInstructions.map(evals(_)(state).pos).contains(evals(instructions)(state).pos)
      }
    }

  property("If there is a path from A to B then there is a path from B to A") =
    forAll { (grid: Grid) =>
      import grid.*
      given Arbitrary[Position] = arbitrary_Position(grid)
      forAll { (rawObstacles: Set[Position], a: Position, b: Position) =>
        val obstacles = rawObstacles - a - b
        val pathAB = dijkstra(b, obstacles, Map(a -> List.empty))
        val pathBA = dijkstra(a, obstacles, Map(b -> List.empty))
        pathAB.isDefined == pathBA.isDefined
      }
    }

  property("If there are no obstacles there must always be a path") =
    forAll { (grid: Grid) =>
      import grid.*
      given Arbitrary[Position] = arbitrary_Position(grid)
      forAll { (a: Position, b: Position) =>
        dijkstra(b, Set.empty, Map(a -> List.empty)).isDefined
      }
    }

  property("There is always a path to where you currently are") =
    forAll { (grid: Grid) =>
      import grid.*
      given Arbitrary[Position] = arbitrary_Position(grid)
      forAll { (rawObstacles: Set[Position], a: Position) =>
        val obstacles = rawObstacles - a
        dijkstra(a, obstacles, Map(a -> List.empty)).isDefined
      }
    }

  property("If the target destination is in the list of obstacles then you cannot get there unless you already are") =
    forAll { (grid: Grid) =>
      import grid.*
      given Arbitrary[Position] = arbitrary_Position(grid)
      forAll { (rawObstacles: Set[Position], a: Position, b: Position) =>
        val obstacles = rawObstacles - a + b
        dijkstra(b, obstacles, Map(a -> List.empty)).isDefined == (a == b)
      }
    }

  property("If we can find a program then following it must get us to our destination ") =
    forAll { (grid: Grid) =>
      import grid.*
      given Arbitrary[Position] = arbitrary_Position(grid)
      given Arbitrary[State] = arbitrary_State(grid)
      forAll { (state: State, obstacles: Set[Position], target: Position) =>
        findShortestProgram(state, target, obstacles).forall(evals(_)(state).pos == target)
      }
    }

  property("Given a grid divided in four sectors, you can access all but the obstacles (by wrapping around)") =
    forAll { (grid: Grid) =>
      import grid.*
      given Arbitrary[Position] = arbitrary_Position(grid)
      given Arbitrary[State] = arbitrary_State(grid)
      forAll { (state: State, target: Position) =>
        val obstacles = Set(
          Range(0, grid.width).map(Position.wrap(_, grid.height / 2)),
          Range(0, grid.height).map(Position.wrap(grid.width / 2, _)),
        ).flatten
        obstacles.contains(target)
          || obstacles.contains(state.pos)
          || findShortestProgram(state, target, obstacles).isDefined
      }
    }
