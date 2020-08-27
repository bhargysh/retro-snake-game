package snake

import cats.data.{ReaderT, State}

sealed trait FoodAction

case object GrowSnake extends FoodAction
case object AddFood extends FoodAction
case object FoodReady extends FoodAction
case class MovedSnake(newSnake: Snake) extends FoodAction

object FoodAction {
  type Play[A] = ReaderT[P, Board, A]
  type P[A] = State[PlayState, A] //ReaderT[State[PlayState, A], Board, A] where A -> Unit

  def modifyState(f: PlayState => PlayState): Play[Unit] = {
    ReaderT.liftF[P, Board, Unit](State.modify[PlayState](f))
    //          F[_] -> underlying monad
    //          A -> extram params it needs
    //          B -> return type
  }

  def inspectState[S](f: PlayState => S): Play[S] = {
    ReaderT.liftF[P, Board, S](State.inspect(f))
  }

  def isPlayingCurrently(snake: Snake): Play[Boolean] = for {
      board <- ReaderT.ask[P, Board]
    } yield !board.isWall(snake.location.head)

  def execute(foodAction: FoodAction, moveNumber: MoveNumber, food: Food): Play[Vector[FoodAction]] = {
    foodAction match {
      case GrowSnake => for {
      x <- modifyState(playState => playState.copy(snake = playState.snake.copy(length = playState.snake.length + 1)))
      } yield Vector.empty[FoodAction]
      case AddFood => for {
      _ <- modifyState(_.copy(food = FoodAbsent(turns = moveNumber + 10)))
      } yield Vector.empty[FoodAction]
      case FoodReady => for {
        board <- ReaderT.ask[P, Board]
        _ <- modifyState(playState => playState.copy(food = playState.foodGenerator.apply(moveNumber, playState.snake, board)))
      } yield Vector.empty[FoodAction]
      case MovedSnake(newSnake) => for {
        playing <- isPlayingCurrently(newSnake)
        newActions = food.eat(newSnake.location.head, moveNumber)
        _ <- modifyState(playState => playState.copy(playing = playing, snake = newSnake))
      } yield newActions
    }
  }
}