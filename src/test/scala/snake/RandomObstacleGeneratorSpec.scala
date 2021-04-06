package snake

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import Generators._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.scalacheck.Parameters

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

    "generate obstacle that is not on the snake, food or wall" >> prop { (snake: Snake, food: FoodPresent, board: Board, obstacles: Set[Location]) =>
      val obstacleGenerator = new RandomObstacleGenerator(new Random)
      val result: Location = obstacleGenerator.apply(food, snake, board, obstacles)
      snake.location should not(contain(result))
      result shouldNotEqual food.location
      board.cellAt(result) shouldEqual EmptyCell
      obstacles should not(contain(result))
    }.setParameters(Parameters(minTestsOk = 400))
  }
}
