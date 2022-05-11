# Running

All three part of the challenge were implemented.
Due to the time constraints I only ran some basic manual tests.
However, there are some ideas for property based tests in the next section.
To build and run the program on the example scenario, run `sbt run`.

# Property based tests

- Found paths must be realisable
  
  If `shortestPath(grid, source, target, obstacles) = Some(path)` then there must exist some `commands` such that `pathToCommands(rover, path) = Some(commands)` and `execCommands(rover)(commands)(source) = target`.

- There always exists a path to itself

  For any `source` and `obstacles`, `shortestPath(grid, source, source, obstacles) = Some(List(source))`

- Target on obstacle
  
  For any `source` and `target` such that `source != target`, if `obstacles.contains(target) = True` then `shortestPath(grid, source, target, obstacles) = None`

- Wrapping

  There exists a path from `s` to `t` irrespective of what quadrants they are on.
  ```
          |
       s  |
          |
    -------------
          |
          |   t
          |
  ```
