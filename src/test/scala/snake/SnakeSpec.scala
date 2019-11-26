package snake

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments
import org.scalacheck.Arbitrary.arbitrary
import Generators._
import org.specs2.ScalaCheck

class SnakeSpec extends Specification with ScalaCheck {
  "Snake" >> {
    "validateDirection" should {
      "should turn towards other axis" in {
        val listOfDirections = List((Up, Left), (Down, Right), (Right, Up), (Left, Down),
          (Up, Right), (Down, Left), (Right, Down), (Left, Up))
        Fragments.foreach(listOfDirections) { case (oldDirection, newDirection) =>
          s"when snake is facing ${oldDirection}" in {
            val snake = Snake(List(Location(2, 2), Location(2, 3)), 2, oldDirection)
            snake.validateDirection(newDirection) must beTrue
          }
        }
      }
      "should not turn in the opposite direction" in {
        val listOfDirections = List((Up, Down), (Down, Up), (Right, Left), (Left, Right))
        Fragments.foreach(listOfDirections) { case (oldDirection, newDirection) =>
          s"when snake is facing ${oldDirection}" in {
            val snake = Snake(List(Location(2, 2), Location(2, 3)), 2, oldDirection)
            snake.validateDirection(newDirection) must beFalse
          }
        }
      }
      "should not turn in the same direction" in {
        val listOfDirections = List(Up, Down, Right, Left)
        Fragments.foreach(listOfDirections) { direction =>
          s"when snake is facing ${direction}" in {
            val snake = Snake(List(Location(2, 2), Location(2, 3)), 2, direction)
            snake.validateDirection(direction) must beFalse
          }
        }
      }
    }
    "forward" should {
      implicit val arbSnake: Arbitrary[Snake] = {
        def createLocation(location: List[Location], length: Int): Gen[List[Location]] = for {
          nextLocation <- arbitrary[Location] //TODO: test for adjacent locations
          newLocation = nextLocation :: location
          result <- if(newLocation.length < length) Gen.frequency((4, createLocation(newLocation, length)), (1, Gen.const(newLocation))) else Gen.const(newLocation)
        } yield result

        Arbitrary(
          for {
            firstLocation <- arbitrary[Location]
            length <- Gen.choose(2, 10)
            location <- createLocation(List(firstLocation), length)
            direction <- arbitrary[Direction]
          } yield Snake(location, length, direction)
        )
      }
      "location is no longer than length of snake" >> prop { snake: Snake =>
        snake.forward().location must haveSize(beLessThanOrEqualTo(snake.length))
      }
    }
  }

}
