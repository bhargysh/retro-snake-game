package snake.components

import org.specs2.mutable.Specification
import snake.{FoodAbsent, FoodPresent, Location, MoveNumber, SnakeGameHtml, SnakeGameWorld}

class GameBoardSpec extends Specification with PlayStateFixtures {

  def findNodesWithText(element: Element, text: String): Vector[Element] = {
    element.children.flatMap {
      case TextNode(data) if data == text => Vector(element)
      case ElementNode(childElement) => findNodesWithText(childElement, text)
      case _ => Vector.empty
    }
  }
  def findNodesWithText(elems: Vector[Element], text: String): Vector[Element] = {
    elems.flatMap(elem => findNodesWithText(elem, text))
  }
  "GameBoard" should {
    "render the snake" in {
      val gameBoard = GameBoard(SnakeGameWorld.newSnakeGameWorld)
      val elements = gameBoard.render()
      findNodesWithText(elements, "🐍") must haveLength(2)
    }
    "render the food if it should be visible" in {
      val snakeGameWorldWithFood = modifyPlayState(_.copy(food = FoodPresent(Location(1, 1), MoveNumber(200))))
      val gameBoard = GameBoard(snakeGameWorldWithFood)
      val elements = gameBoard.render()
      findNodesWithText(elements, "🍕") must contain((e: Element) => {
        e.style must beSome("grid-column: 2; grid-row: 9;") // (1,1) is equivalent to the css grid's 2, 9
        e.classes must beEmpty
      })
    }
    "render disappearing food if it should expire soon" in {
      val snakeGameWorldWithFood = modifyPlayState(_.copy(food = FoodPresent(Location(1, 1), MoveNumber(1))))
      val gameBoard = GameBoard(snakeGameWorldWithFood)
      val elements = gameBoard.render()
      findNodesWithText(elements, "🍕") must contain((e: Element) => e.classes mustEqual Vector("food-disappearing"))
    }
    "render no food if it is not present" in {
      val snakeGameWorldWithFood = modifyPlayState(_.copy(food = FoodAbsent(MoveNumber(1))))
      val gameBoard = GameBoard(snakeGameWorldWithFood)
      val elements = gameBoard.render()
      findNodesWithText(elements, "🍕") must beEmpty
    }
    "render the game over sign if user is no longer playing" in {
      val snakeGameWorldNotPlaying = modifyPlayState(_.copy(playing = false))
      val gameBoard = GameBoard(snakeGameWorldNotPlaying)
      val elements = gameBoard.render()
      findNodesWithText(elements, "GAME OVER") must not(beEmpty)
    }
    "not render the game over sign if user playing" in {
      val snakeGameWorldPlaying = modifyPlayState(_.copy(playing = true))
      val gameBoard = GameBoard(snakeGameWorldPlaying)
      val elements = gameBoard.render()
      findNodesWithText(elements, "GAME OVER") must beEmpty
    }
    "renders obstacle" in {
      val snakeGameWorldObstacle = modifyPlayState(_.copy(obstacles = Set(Location(2,2))))
      val gameBoard = GameBoard(snakeGameWorldObstacle)
      val elements = gameBoard.render()

      findNodesWithText(elements, "\ud83d\udea8") must haveLength(1)
    }
  }

}
