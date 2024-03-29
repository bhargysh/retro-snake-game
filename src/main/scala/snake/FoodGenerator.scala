package snake

import cats.effect.IO

import scala.util.Random

trait FoodGenerator[F[_]] {
  def apply(moveNumber: MoveNumber, snake: Snake, board: Board, obstacles: Set[Location]): F[FoodPresent]
}

class RandomFoodGenerator(randomNumberGenerator: Random) extends FoodGenerator[IO] {

  def apply(moveNumber: MoveNumber, snake: Snake, board: Board, obstacles: Set[Location]): IO[FoodPresent] = IO {
    val notAvailable: Set[Location] = snake.location.toSet ++ obstacles
    val emptyLocations: Set[Location] = board.locations.filter { location =>
      board.cellAt(location) == EmptyCell
    }
    val availableLocations: Set[Location] = emptyLocations -- notAvailable
    val randomLocation = randomNumberGenerator.nextInt(availableLocations.size)
    val delay = randomNumberGenerator.nextInt(5) + 5
    FoodPresent(availableLocations.toSeq.apply(randomLocation), moveNumber + delay)
  }
}
