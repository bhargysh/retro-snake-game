import cats.{ApplicativeError, Monad}
import cats.data.{IndexedStateT, Kleisli, ReaderT, State, StateT}
import cats.effect.{Bracket, ExitCase, IO, Sync}

package object snake { //want something at top level, cannot usefully define it inside another object

//  type App[A] = ReaderT[AppState, Board, A] // A => result type
//  type AppState[A] = State[(PlayState, MoveNumber), A] // this is because State has (S,A) needs two types

  type TurnEnv = (Board, MoveNumber)
  type P[A] = StateT[IO, TurnState, A]
  type Turn[A] = ReaderT[P, TurnEnv, A]
  type Game[A] = StateT[IO, SnakeGameWorld, A]
//  type P = Lambda[(F[_], A) => StateT[F, PlayState, A]]
//  type Play[A] = ReaderT[P, PlayEnv, A]

  implicit def readerTforStuff: Sync[Turn] = new Sync[Turn] {

    def suspend[A](thunk: => Turn[A]): Turn[A] = ReaderT
      .liftF[StateT[IO, TurnState, *], (Board, MoveNumber), Turn[A]](StateT.liftF[IO, TurnState, Turn[A]](IO.suspend(IO[Turn[A]](thunk)))).flatMap(identity)

    def bracketCase[A, B](acquire: Turn[A])(use: A => Turn[B])(release: (A, ExitCase[Throwable]) => Turn[Unit]): Turn[B] = {
      Bracket.catsKleisliBracket[P, (Board, MoveNumber), Throwable](Sync[P]).bracketCase(acquire)(use)(release)
    }

    def raiseError[A](e: Throwable): Turn[A] = ReaderT
      .liftF[StateT[IO, TurnState, *], (Board, MoveNumber), A](StateT.liftF[IO, TurnState, A](IO.raiseError(e)))

    def handleErrorWith[A](fa: Turn[A])(f: Throwable => Turn[A]): Turn[A] = Kleisli { env: (Board, MoveNumber) =>
      val pOfA: P[A] = fa.run(env)
      ApplicativeError[P, Throwable].handleErrorWith(pOfA)(throwable => f(throwable).run(env))
    }

    //A -> IO[B], get to StateT IO[B]

    def pure[A](x: A): Turn[A] = ReaderT.liftF[StateT[IO, TurnState, *], (Board, MoveNumber), A](StateT.liftF[IO, TurnState, A](IO.pure[A](x)))

    def flatMap[A, B](fa: Turn[A])(f: A => Turn[B]): Turn[B] = fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Turn[Either[A, B]]): Turn[B] = {
      Kleisli.catsDataMonadForKleisli[P, (Board, MoveNumber)](IndexedStateT.catsDataMonadForIndexedStateT[IO, TurnState](Monad[IO])).tailRecM(a)(f)
    }
  }
}
