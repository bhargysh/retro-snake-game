import cats.{ApplicativeError, Monad}
import cats.data.{IndexedStateT, Kleisli, ReaderT, State, StateT}
import cats.effect.{Bracket, ExitCase, IO, Sync}

package object snake { //want something at top level, cannot usefully define it inside another object

//  type App[A] = ReaderT[AppState, Board, A] // A => result type
//  type AppState[A] = State[(PlayState, MoveNumber), A] // this is because State has (S,A) needs two types

  type PlayEnv = (Board, MoveNumber) //TurnEnv
  type P[A] = StateT[IO, PlayState, A] // TurnState
  type Play[A] = ReaderT[P, PlayEnv, A] // Turn
  type Game[A] = StateT[IO, SnakeGameWorld, A] //TODO: rename things
//  type P = Lambda[(F[_], A) => StateT[F, PlayState, A]]
//  type Play[A] = ReaderT[P, PlayEnv, A]

  implicit def readerTforStuff: Sync[Play] = new Sync[Play] {

    def suspend[A](thunk: => Play[A]): Play[A] = ReaderT
      .liftF[StateT[IO, PlayState, *], (Board, MoveNumber), Play[A]](StateT.liftF[IO, PlayState, Play[A]](IO.suspend(IO[Play[A]](thunk)))).flatMap(identity)

    def bracketCase[A, B](acquire: Play[A])(use: A => Play[B])(release: (A, ExitCase[Throwable]) => Play[Unit]): Play[B] = {
      Bracket.catsKleisliBracket[P, (Board, MoveNumber), Throwable](Sync[P]).bracketCase(acquire)(use)(release)
    }

    def raiseError[A](e: Throwable): Play[A] = ReaderT
      .liftF[StateT[IO, PlayState, *], (Board, MoveNumber), A](StateT.liftF[IO, PlayState, A](IO.raiseError(e)))

    def handleErrorWith[A](fa: Play[A])(f: Throwable => Play[A]): Play[A] = Kleisli { env: (Board, MoveNumber) =>
      val pOfA: P[A] = fa.run(env)
      ApplicativeError[P, Throwable].handleErrorWith(pOfA)(throwable => f(throwable).run(env))
    }

    //A -> IO[B], get to StateT IO[B]

    def pure[A](x: A): Play[A] = ReaderT.liftF[StateT[IO, PlayState, *], (Board, MoveNumber), A](StateT.liftF[IO, PlayState, A](IO.pure[A](x)))

    def flatMap[A, B](fa: Play[A])(f: A => Play[B]): Play[B] = fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Play[Either[A, B]]): Play[B] = {
      Kleisli.catsDataMonadForKleisli[P, (Board, MoveNumber)](IndexedStateT.catsDataMonadForIndexedStateT[IO, PlayState](Monad[IO])).tailRecM(a)(f)
    }
  }
}
