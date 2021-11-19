package snake

import cats.data.StateT
import cats.effect.IO

object GameState {

  def toGameState(turn: Turn[Unit])(implicit boardActionStateReader: BoardActionStateReader[Turn]): Game[SnakeGameWorld] = {
    def newSnakeGameWorld: Turn[SnakeGameWorld] =
      for {
        board <- boardActionStateReader.askForBoard
        moveNumber <- boardActionStateReader.askForMoveNumber
        newWorld <- boardActionStateReader.inspectState { turnState =>
          SnakeGameWorld(board, moveNumber + 1, turnState)
        }
      } yield newWorld

    StateT.modifyF[IO, SnakeGameWorld] {
      case SnakeGameWorld(board, moveNumber, turnState) =>
        turn
          .flatMap(_ => newSnakeGameWorld)
          .run((board, moveNumber))
          .runA(turnState)
    }.get
  }

}
