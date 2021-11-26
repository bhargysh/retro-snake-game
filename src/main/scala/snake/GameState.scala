package snake

import cats.data.StateT
import cats.effect.IO

class GameState(actionRunner: ActionRunner[Turn, BoardAction]) {
  /*
  transformS() takes the P state and morphs it to Game using 2 functions. Like a getter & setter.
  1. First function: takes the SNG and maps it to TurnState (getter)
  2. Second function: takes the SNG and evluated TurnState, returning a new SNG (setter)
  */

  def playTurn(direction: Option[Direction]): Game[SnakeGameWorld] = {
    for {
      board <- StateT.inspect[IO, SnakeGameWorld, Board](sng => sng.board)
      moveNumber <- StateT.inspect[IO, SnakeGameWorld, MoveNumber](sng => sng.moveNumber)
      newSNG <- actionRunner.runActions(Vector(StartTurn(direction)))
        .run((board, moveNumber))
        .transformS[SnakeGameWorld](sng => sng.turnState, (sng, newTS) => SnakeGameWorld(board, moveNumber + 1, newTS))
        .get
    } yield newSNG
  }

}
