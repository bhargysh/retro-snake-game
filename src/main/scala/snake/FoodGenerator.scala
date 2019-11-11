package snake

import scala.util.Random

class FoodGenerator(randomNumberGenerator: Random) {

  def apply(moveNumber: Int, snake: Snake, board: Board): Food = {
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
