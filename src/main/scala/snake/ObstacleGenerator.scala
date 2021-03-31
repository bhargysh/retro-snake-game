package snake

import scala.util.Random

trait ObstacleGenerator {
  def apply(food: Food, snake: Snake, board: Board): Location
}


class RandomObstacleGenerator(randomNumberGenerator: Random) extends ObstacleGenerator {
  def apply(food: Food, snake: Snake, board: Board): Location = {
    val snakeLocation: Set[Location] = snake.location.toSet
    val emptyLocations: Set[Location] = board.locations.filter { location =>
      board.cellAt(location) == EmptyCell
    }
    val foodLocation: Set[Location] = food match {
      case FoodPresent(location, _) => Set(location)
      case FoodAbsent(_) => Set.empty[Location]
    }
    val notAvailable = snakeLocation ++ foodLocation
    val availableLocations = (emptyLocations -- notAvailable).toVector
    val position = randomNumberGenerator.nextInt(availableLocations.size)
    availableLocations(position)
  }
}