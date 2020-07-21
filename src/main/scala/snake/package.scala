import cats.data.{ReaderT, State}

package object snake { //want something at top level, cannot usefully define it inside another object

  type App[A] = ReaderT[AppState, Board, A] // A => result type
  type AppState[A] = State[(PlayState, MoveNumber), A] // this is because State has (S,A) needs two types

}
