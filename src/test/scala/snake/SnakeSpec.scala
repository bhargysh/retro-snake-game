package snake

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments
import org.scalacheck.Arbitrary.arbitrary
import Generators._
import org.specs2.ScalaCheck

class SnakeSpec extends Specification with ScalaCheck {
  "Snake" >> {
    "turn" should {
      "should turn towards other axis" in {
        val listOfDirections = List((Up, Left), (Down, Right), (Right, Up), (Left, Down),
          (Up, Right), (Down, Left), (Right, Down), (Left, Up))
        Fragments.foreach(listOfDirections) { case (oldDirection, newDirection) =>
          s"when snake is facing ${oldDirection}" in {
            val snake = Snake(List(Location(2, 2), Location(2, 3)), 2, oldDirection)
            snake.turn(newDirection).direction mustEqual newDirection
          }
        }
      }
      "should not turn in the opposite direction" in {
        val listOfDirections = List((Up, Down), (Down, Up), (Right, Left), (Left, Right))
        Fragments.foreach(listOfDirections) { case (oldDirection, newDirection) =>
          s"when snake is facing ${oldDirection}" in {
            val snake = Snake(List(Location(2, 2), Location(2, 3)), 2, oldDirection)
            snake.turn(newDirection).direction mustEqual oldDirection
          }
        }
      }
      "should not turn in the same direction" in {
        val listOfDirections = List(Up, Down, Right, Left)
        Fragments.foreach(listOfDirections) { direction =>
          s"when snake is facing ${direction}" in {
            val snake = Snake(List(Location(2, 2), Location(2, 3)), 2, direction)
            snake.turn(direction).direction mustEqual direction
          }
        }
      }
    }
    "forward" should {
      implicit val arbSnake: Arbitrary[Snake] = {
        def adjacentLocations(location: Location): List[Location] = {
          List(location.copy(x = location.x + 1), location.copy(x = location.x - 1), location.copy(y = location.y + 1),  location.copy(y = location.y - 1))
        }

        def createLocation(location: List[Location], length: Int): Gen[List[Location]] = for {
          possibleNextLocations <- Gen.const(adjacentLocations(location.head))
          result <- possibleNextLocations.filter(l => !location.contains(l)) match {
            case Nil => Gen.const(location)
            case nonEmpty => for {
              nextLocation <- Gen.oneOf(nonEmpty)
              newLocation = nextLocation :: location
              result <- if (location.length < length) {
                Gen.frequency((4, createLocation(newLocation, length)), (1, Gen.const(newLocation)))
              } else {
                Gen.const(newLocation)
              }
            } yield result
          }
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
      "does not change the length or direction" >> prop { snake: Snake =>
        snake.forward().length must beEqualTo(snake.length)
        snake.forward().direction must beEqualTo(snake.direction)
      }
      "new location's tail is in the old location" >> prop { snake: Snake =>
        snake.forward().location.tail must ((l: List[Location]) => l.forall(snake.location.contains))
      }
    }
  }

}
