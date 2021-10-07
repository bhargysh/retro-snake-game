package snake

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.matcher.IOMatchers
import org.specs2.mutable.Specification
import snake.Generators._

import scala.util.Random

class RandomFoodGeneratorSpec extends Specification with ScalaCheck with IOMatchers {
  "Random food generator" should {
    implicit val snakeArb = Arbitrary(Generators.snakeGen.filter(snake => !snake.location.contains(Location(3, 3))))

    implicit val orderingMoveNumber: Ordering[MoveNumber] = Ordering[Int].on((n: MoveNumber) => n.number)

    "generate food that is not on the snake, wall, or obstable" >> prop { (moveNumber: MoveNumber, snake: Snake) =>
      val food = new RandomFoodGenerator(new Random())
      val foodLocation = Location(3,3)
      val obstacleLocations = for {
        x <- Range.inclusive(1, 8) //excluded wall
        y <- Range.inclusive(1, 8)
        l = Location(x, y)
        if !snake.location.contains(l) && l != foodLocation
      } yield l
      val result = food(moveNumber, snake, SnakeGameWorld.board, obstacleLocations.toSet)
      result should returnValue(beLike[Food]{ case FoodPresent(location, expiryTime) =>
        expiryTime should beBetween(moveNumber + 5, moveNumber + 10).excludingEnd
        snake.location should not(contain(location))
        location.x should beBetween(1, 8)
        location.y should beBetween(1, 8)
        obstacleLocations should not(contain(location))
      })
    }
  }

}
