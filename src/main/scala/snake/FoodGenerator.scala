package snake

import scala.util.Random

trait FoodGenerator {
  def apply(moveNumber: MoveNumber, snake: Snake, board: Board): FoodPresent
}

class RandomFoodGenerator(randomNumberGenerator: Random) extends FoodGenerator {

  def apply(moveNumber: MoveNumber, snake: Snake, board: Board): FoodPresent = {
    val notAvailable: Set[Location] = snake.location.toSet
    val emptyLocations: Set[Location] = board.locations.filter { location =>
      board.cellAt(location) == EmptyCell
    }
    val availableLocations: Set[Location] = emptyLocations -- notAvailable
    val randomLocation = randomNumberGenerator.nextInt(availableLocations.size)
    val delay = randomNumberGenerator.nextInt(5) + 5
    FoodPresent(availableLocations.toSeq.apply(randomLocation), moveNumber + delay)
  }
}
