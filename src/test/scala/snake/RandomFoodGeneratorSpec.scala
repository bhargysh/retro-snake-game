package snake

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.Random
import Generators._

class RandomFoodGeneratorSpec extends Specification with ScalaCheck {
  "Random food generator" should {
    implicit val snakeArb = Arbitrary(Generators.snakeGen)
    "generate food that is not on the snake or wall" >> prop { (moveNumber: MoveNumber, snake: Snake) =>
      val food = new RandomFoodGenerator(new Random())
      val result: Food = food(moveNumber, snake, SnakeGameWorld.board) //empty board as it does not change
      result should beLike[Food]{ case FoodPresent(location, expiryTime) =>
        expiryTime should beBetween(moveNumber + 5, moveNumber + 10).excludingEnd
        snake.location should not(contain(location))
        location.x should beBetween(1, 8)
        location.y should beBetween(1, 8)
      }
    }
  }

}
