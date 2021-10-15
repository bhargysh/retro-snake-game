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
  implicit def arbPlay[A: Arbitrary]: Arbitrary[Turn[A]] = Arbitrary(for {
    a <- Arbitrary.arbitrary[A]
  } yield a.pure[Turn]
  )

  val initialEnv: TurnEnv = (SnakeGameWorld.board, MoveNumber(0))

  implicit val eqPlayState: Eq[TurnState] = Eq.fromUniversalEquals[TurnState]

  implicit def eqFa[A: Eq]: Eq[Turn[A]] = (x: Turn[A], y: Turn[A]) => {
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

  val eqEitherT: Eq[EitherT[Turn, Throwable, Int]] = implicitly[Eq[EitherT[Turn, Throwable, Int]]]
  implicit lazy val iso: SemigroupalTests.Isomorphisms[Turn] = SemigroupalTests.Isomorphisms.invariant

  "sync laws" should {
    implicit val eqEitherT2: Eq[EitherT[Turn, Throwable, Int]] = eqEitherT
    checkAll("Play", SyncTests.apply[Turn].sync[Int, Int, String])
    // run testOnly *SyncPlayLawsSpec -- ex "adaptError raise"
  }

}
