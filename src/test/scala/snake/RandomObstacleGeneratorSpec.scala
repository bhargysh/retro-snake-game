package snake

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import Generators._
import org.scalacheck.Arbitrary

class RandomObstacleGeneratorSpec extends Specification with ScalaCheck {
  "Random obstacle generator" should {
    implicit val snakeArb = Arbitrary(Generators.snakeGen)
    "generate obstacle that is not on the snake, food or wall" >> prop { (moveNumber: MoveNumber, snake: Snake, food: FoodPresent) =>
      //TODO: START HERE!
      ko
    }
  }
}
