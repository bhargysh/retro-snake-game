package snake

import cats.data.EitherT
import cats.effect.laws.discipline.SyncTests
import cats.effect.laws.util.TestInstances._
import cats.implicits._
import cats.kernel.Eq
import cats.laws.discipline.SemigroupalTests
import org.scalacheck.Arbitrary
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

class SyncPlayLawsSpec extends Specification with Discipline with BoardActionFixtures {
  implicit def arbPlay[A: Arbitrary]: Arbitrary[Play[A]] = Arbitrary(for {
    a <- Arbitrary.arbitrary[A]
  } yield a.pure[Play]
  )

  val initialEnv: PlayEnv = (SnakeGameWorld.board, MoveNumber(0))

  implicit val eqPlayState: Eq[TurnState] = Eq.fromUniversalEquals[TurnState]

  implicit def eqFa[A: Eq]: Eq[Play[A]] = (x: Play[A], y: Play[A]) => {
    val a = x
      .run(initialEnv)
      .run(initialPlayState)
      .attempt
      .unsafeRunSync()
    val a2 = y
      .run(initialEnv)
      .run(initialPlayState)
      .attempt
      .unsafeRunSync()
    Eq[Either[Throwable, (TurnState, A)]].eqv(a, a2)
  }

  val eqEitherT: Eq[EitherT[Play, Throwable, Int]] = implicitly[Eq[EitherT[Play, Throwable, Int]]]
  implicit lazy val iso: SemigroupalTests.Isomorphisms[Play] = SemigroupalTests.Isomorphisms.invariant

  "sync laws" should {
    implicit val eqEitherT2: Eq[EitherT[Play, Throwable, Int]] = eqEitherT
    checkAll("Play", SyncTests.apply[Play].sync[Int, Int, String])
    // run testOnly *SyncPlayLawsSpec -- ex "adaptError raise"
  }

}
