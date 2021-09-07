package snake

import cats.effect.IO

import scala.util.Random

trait ObstacleGenerator[F[_]] {
  def apply(food: Food, snake: Snake, board: Board, obstacles: Set[Location]): IO[Location]
}


class RandomObstacleGenerator(randomNumberGenerator: Random) extends ObstacleGenerator[IO] {
  def apply(food: Food, snake: Snake, board: Board, obstacles: Set[Location]): IO[Location] = IO {
    val snakeLocation: Set[Location] = snake.location.toSet
    val emptyLocations: Set[Location] = board.locations.filter { location =>
      board.cellAt(location) == EmptyCell
    }
    val foodLocation: Set[Location] = food match {
      case FoodPresent(location, _) => Set(location)
      case FoodAbsent(_) => Set.empty[Location]
    }
    val notAvailable = snakeLocation ++ foodLocation ++ obstacles
    val availableLocations = (emptyLocations -- notAvailable).toVector
    val position = randomNumberGenerator.nextInt(availableLocations.size)
    availableLocations(position)
  }

  override def equals(obj: Any): Boolean = obj.getClass == this.getClass // TODO: this is to test SyncLaws remove this later
}