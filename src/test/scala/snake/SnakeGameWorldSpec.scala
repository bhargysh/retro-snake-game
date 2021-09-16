package snake

import cats.implicits._
import org.specs2.mutable.Specification

class SnakeGameWorldSpec extends Specification with BoardActionFixtures {
  "play" should {
    "return a new SnakeGameWorld" in {
      implicit val actionRunner: ActionRunner[Play, BoardAction] = new ActionRunner[Play, BoardAction] {
        def runActions(actions: Vector[BoardAction]): Play[Unit] = ().pure[Play]
      }
      val snake = Snake(
        List(Location(1, 1), Location(1, 2)),
        length = 2,
        direction = Up
      )
      val initialSNG = SnakeGameWorld(
        snake,
        SnakeGameWorld.obstacles,
        SnakeGameWorld.board,
        SnakeGameWorld.food,
        isPlaying = true,
        MoveNumber(1)
      )
      val updatedSNG: SnakeGameWorld =
        initialSNG.play[Play](direction = Some(Up))
          .run(initialSNG.board, initialSNG.moveNumber)
          .runA(initialPlayState)
          .unsafeRunSync()

      updatedSNG.moveNumber shouldEqual MoveNumber(2) //TODO: what should this test be testing?
    }
  }
}
