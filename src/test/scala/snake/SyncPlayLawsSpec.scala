package snake

import cats.data.EitherT
import cats.effect.laws.discipline.SyncTests
import cats.implicits._
import cats.kernel.Eq
import org.scalacheck.Arbitrary
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import cats.effect.laws.util.TestInstances._
import cats.laws.discipline.SemigroupalTests

class SyncPlayLawsSpec extends Specification with Discipline with BoardActionFixtures {
  implicit def arbPlay[A: Arbitrary]: Arbitrary[Play[A]] = Arbitrary(for {
    a <- Arbitrary.arbitrary[A]
  } yield a.pure[Play]
  )

  val initialEnv = (SnakeGameWorld.board, MoveNumber(0))

  implicit def eqFa[A: Eq]: Eq[Play[A]] = (x: Play[A], y: Play[A]) => {
    val (_, a) = x
      .run(initialEnv)
      .run(initialPlayState)
      .unsafeRunSync()
    val (_, a2) = y
      .run(initialEnv)
      .run(initialPlayState)
      .unsafeRunSync()
    Eq[A].eqv(a, a2)
  }

  val eqEitherT: Eq[EitherT[Play, Throwable, Int]] = implicitly[Eq[EitherT[Play, Throwable, Int]]]
  implicit lazy val iso: SemigroupalTests.Isomorphisms[Play] = SemigroupalTests.Isomorphisms.invariant

  "hi" should {
    implicit val eqEitherT2 = eqEitherT
    checkAll("Play", SyncTests.apply[Play].sync[Int, Int, String]) //TODO: start with looking at ApplicativeErrorTests failing tests (on error raise)
  }

}
