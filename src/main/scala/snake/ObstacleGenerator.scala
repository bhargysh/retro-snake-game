package snake

import scala.util.Random

trait ObstacleGenerator {
  def apply(moveNumber: MoveNumber, snake: Snake, board: Board): Location
}


class RandomObstacleGenerator(randomNumberGenerator: Random) extends ObstacleGenerator {
  def apply(moveNumber: MoveNumber, snake: Snake, board: Board): Location = {
    val notAvailable: Set[Location] = snake.location.toSet
    val emptyLocations: Set[Location] = board.locations.filter { location =>
      board.cellAt(location) == EmptyCell
    }
    val availableLocations = (emptyLocations -- notAvailable).toVector
    val position = randomNumberGenerator.nextInt(availableLocations.size)
    availableLocations(position)
  }
}