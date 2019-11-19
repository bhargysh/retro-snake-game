package snake

import org.scalacheck.{Arbitrary, Gen}

object Generators {
  implicit val arbDirection: Arbitrary[Direction] = Arbitrary(Gen.oneOf(Up, Down, Left, Right))
  implicit val arbLocation: Arbitrary[Location] = Arbitrary(
    for {
      x <- Gen.choose(1,8)
      y <- Gen.choose(1, 8)
    } yield Location(x, y)
  )
}
