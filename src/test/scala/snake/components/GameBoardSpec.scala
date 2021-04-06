package snake.components

import org.specs2.mutable.Specification
import snake.{FoodAbsent, FoodPresent, Location, MoveNumber, SnakeGameHtml, SnakeGameWorld}

class GameBoardSpec extends Specification {

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
  "SnakeGameHtml" should {
    "render the snake" in {
      val gameBoard = GameBoard(SnakeGameWorld.newSnakeGameWorld)
      val elements = gameBoard.render()
      findNodesWithText(elements, "🐍") must haveLength(2)
    }
    "render the food if it should be visible" in {
      val snakeGameWorldWithFood = SnakeGameWorld.newSnakeGameWorld.copy(food = FoodPresent(Location(1, 1), MoveNumber(200)))
      val gameBoard = GameBoard(snakeGameWorldWithFood)
      val elements = gameBoard.render()
      findNodesWithText(elements, "🍕") must contain((e: Element) => {
        e.style must beSome("grid-column: 2; grid-row: 9;") // (1,1) is equivalent to the css grid's 2, 9
      })
    }
    "render no food if it should not be visible" in {
      val snakeGameWorldWithFood = SnakeGameWorld.newSnakeGameWorld.copy(food = FoodPresent(Location(1, 1), MoveNumber(1)))
      val gameBoard = GameBoard(snakeGameWorldWithFood)
      val elements = gameBoard.render()
      findNodesWithText(elements, "🍕") must beEmpty
    }
    "render no food if it is not present" in {
      val snakeGameWorldWithFood = SnakeGameWorld.newSnakeGameWorld.copy(food = FoodAbsent(MoveNumber(1)))
      val gameBoard = GameBoard(snakeGameWorldWithFood)
      val elements = gameBoard.render()
      findNodesWithText(elements, "🍕") must beEmpty
    }
    "render the game over sign if user is no longer playing" in {
      val snakeGameWorldNotPlaying = SnakeGameWorld.newSnakeGameWorld.copy(isPlaying = false)
      val gameBoard = GameBoard(snakeGameWorldNotPlaying)
      val elements = gameBoard.render()
      findNodesWithText(elements, "GAME OVER") must not(beEmpty)
    }
    "not render the game over sign if user playing" in {
      val snakeGameWorldPlaying = SnakeGameWorld.newSnakeGameWorld.copy(isPlaying = true)
      val gameBoard = GameBoard(snakeGameWorldPlaying)
      val elements = gameBoard.render()
      findNodesWithText(elements, "GAME OVER") must beEmpty
    }
    "renders obstacle" in {
      val snakeGameWorldObstacle = SnakeGameWorld.newSnakeGameWorld.copy(obstacles = Set(Location(2,2)))
      val gameBoard = GameBoard(snakeGameWorldObstacle)
      val elements = gameBoard.render()

      findNodesWithText(elements, "\ud83d\udea8") must haveLength(1)
    }
  }

}
