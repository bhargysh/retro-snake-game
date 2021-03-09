package snake

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen, Shrink}

object Generators {
  implicit val arbDirection: Arbitrary[Direction] = Arbitrary(Gen.oneOf(Up, Down, Left, Right))
  implicit val arbLocation: Arbitrary[Location] = Arbitrary(
    for {
      x <- Gen.choose(1,8)
      y <- Gen.choose(1, 8)
    } yield Location(x, y)
  )
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
  val snakeGen: Gen[Snake] =
    for {
      length <- Gen.choose(2,20)
      location <- Gen.containerOfN[List, Location](length, arbitrary[Location])
      direction <- arbitrary[Direction]
    } yield Snake(location, length, direction)

  implicit val moveNumberArb: Arbitrary[MoveNumber] = Arbitrary(
    arbitrary[Int].map(number => MoveNumber(number))
  )
  implicit val foodArb: Arbitrary[FoodPresent] = Arbitrary(
    for {
      location <- arbitrary[Location]
      moveNumber <- arbitrary[MoveNumber]
    } yield FoodPresent(location, moveNumber)
  )
}
