package snake

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.Random

class FoodGeneratorSpec extends Specification with ScalaCheck {
  implicit val shrinkSnake: Shrink[Snake] = Shrink { case Snake(location, length, direction) =>
    if (length < 3) {
      Stream.empty[Snake]
    }
    else {
      val newLocations = Shrink.shrink(location).filter(_.length >= 2) //only valid snakes generated
      newLocations.flatMap { location =>
        Stream(Snake(location, length, direction), Snake(location, location.length, direction))
      }
    }
  }
  implicit val arbDirection: Arbitrary[Direction] = Arbitrary(Gen.oneOf(Up, Down, Left, Right))
  implicit val arbLocation: Arbitrary[Location] = Arbitrary(
    for {
      x <- Gen.choose(1,8)
      y <- Gen.choose(1, 8)
    } yield Location(x, y)
  )
  implicit val snakeGen: Arbitrary[Snake] = Arbitrary(
    for {
    length <- Gen.choose(2,20)
    location <- Gen.containerOfN[List, Location](length, arbitrary[Location])
    direction <- arbitrary[Direction]
    } yield Snake(location, length, direction)
  )
  "Food generator" should {
    "generate food that is not on the snake" >> prop { (moveNumber:Int, snake: Snake) =>
      val food = new FoodGenerator(new Random())
      val result: Food = food(moveNumber, snake, SnakeGameWorld.board) //empty board as it does not change
      result should beLike[Food]{ case FoodPresent(location, expiryTime) =>
        expiryTime should beBetween(moveNumber + 5, moveNumber + 10).excludingEnd
      }
    //TODO: test location (not in the wall or on snake)
    }
  }

}
