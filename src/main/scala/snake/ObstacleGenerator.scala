package snake

trait ObstacleGenerator {
  def apply(moveNumber: MoveNumber, snake: Snake, board: Board): Location
}


class RandomObstacleGenerator extends ObstacleGenerator {
  def apply(moveNumber: MoveNumber, snake: Snake, board: Board): Location = {
    val notAvailable: Set[Location] = snake.location.toSet
    val emptyLocations: Set[Location] = board.locations.filter { location =>
      board.cellAt(location) == EmptyCell
    }
    val availableLocations: Set[Location] = emptyLocations -- notAvailable
    ???
  }
}