package snake

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import Generators._
import org.scalacheck.{Arbitrary, Gen}

import scala.util.Random

class RandomObstacleGeneratorSpec extends Specification with ScalaCheck {
  "Random obstacle generator" should {
    implicit val snakeArb = Arbitrary(Generators.snakeGen)

    val cellGen: Gen[Cell] = Gen.oneOf[Cell](Wall, EmptyCell, ObstacleCell)
    val boardGen: Gen[Board] = for {
      width <-  Gen.choose(5,20)
      height <- Gen.choose(5,20)
      cells <- Gen.listOfN(width * height, cellGen)
    } yield Board(cells.toArray, width, height)
    implicit val board: Arbitrary[Board] = Arbitrary(boardGen)

    "generate obstacle that is not on the snake, food or wall" >> prop { (moveNumber: MoveNumber, snake: Snake, food: FoodPresent, board: Board) =>
      val obstacleGenerator = new RandomObstacleGenerator(new Random)
      val result: Location = obstacleGenerator.apply(moveNumber, snake, board)
      result should beLike[Location]{ case Location(x, y) =>
        snake.location should not(contain(result))
        food.location shouldNotEqual result //TODO: fix generator, result should be on left comparison
        board.cellAt(result) shouldEqual EmptyCell

      }
    }
  }
}
