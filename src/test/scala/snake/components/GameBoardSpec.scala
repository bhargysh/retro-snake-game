package snake.components

import org.specs2.mutable.Specification
import snake.{FoodAbsent, FoodPresent, Location, MoveNumber, SnakeGameHtml, SnakeGameWorld}

class GameBoardSpec extends Specification {

  def findNodesWithText(node: Node, text: String): Vector[Node] = {
    node match {
      case TextNode(data) if data == text => Vector(node)
      case ElementNode(Element(_, _, _, children)) => children.flatMap(findNodesWithText(_, text))
      case _ => Vector.empty
    }
  }
  def findNodesWithText(elems: Vector[Element], text: String): Vector[Node] = {
    elems.flatMap {
      case Element(_, _, _, children) => children.flatMap(findNodesWithText(_, text))
    }
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
      findNodesWithText(elements, "🍕") must contain((n: Node) => {
        ???
//        val parentNode = n.parentNode
//        parentNode.attributes.getNamedItem("style").value mustEqual "grid-column: 2; grid-row: 9;"
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
  }

}
