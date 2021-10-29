package snake.components

import snake.{EmptyCell, FoodCell, FoodPresent, Location, MoveNumber, ObstacleCell, SnakeGameWorld, SnakePart, TurnState, Wall}

import scala.collection.immutable

sealed trait Node
case class TextNode(data: String) extends Node
case class ElementNode(data: Element) extends Node

case class Element(tag: String, classes: Vector[String], style: Option[String], children: Vector[Node])

trait Component {

  def render(): Vector[Element]
}

//TODO: Animate snake so its smoother when it moves

case class Cell(cell: snake.Cell, x: Int, y: Int, boardHeight: Int, classes: Vector[String] = Vector.empty) extends Component { //TODO: can we remove default
  def render(): Vector[Element] = {
    val text = cell match {
      case SnakePart => "🐍"
      case Wall => "🛑"
      case EmptyCell => " "
      case FoodCell => "🍕"
      case ObstacleCell => "\ud83d\udea8"
    }
    Vector(
      Element("div",
      classes,
      Some(s"grid-column: ${x+1}; grid-row: ${boardHeight - y};"),
      Vector(TextNode(text))))
  }
}

case class GameOver(isPlaying: Boolean) extends Component {
  def render(): Vector[Element] = {
    if (!isPlaying) {
      val gameOverTextNode = Element("span", Vector("gameover-span"), None, Vector(TextNode("GAME OVER")))
      Vector(Element("div", Vector("gameover"), None, Vector(ElementNode(gameOverTextNode))))
    }
    else {
      Vector.empty[Element]
    }
  }
}

case class GameBoard(snakeGameWorld: SnakeGameWorld) extends Component {

  private def foodVisible(expiryTime: MoveNumber, moveNumber: MoveNumber): Boolean = {
    val blinkTime = expiryTime - moveNumber
    if (blinkTime > 4) true
    else blinkTime % 2 == 0
  }

//  def blinkFood(expiryTime: MoveNumber, moveNumber: MoveNumber) = {
//    val duration: Int = expiryTime - moveNumber
//  }

  def render(): Vector[Element] = {
    val turnState = snakeGameWorld.turnState
    val currentFoodLocation: Option[(Location, Vector[String])] = turnState.food match {
      case FoodPresent(location, expiryTime) if foodVisible(expiryTime, snakeGameWorld.moveNumber) => Some((location, Vector("food-present")))
      case _ => None
    }

    def convertToApparantCell(x: Int, y: Int): Cell = {
      val location = Location(x, y)

      object InSnake {
        def unapply(location: Location): Option[Cell] = {
          if(turnState.snake.location.contains(location)) Some(Cell(SnakePart, x, y, snakeGameWorld.board.height))
          else None
        }
      } //TODO: Can we move this to Snake? Same with others...

      object InFood {
        def unapply(location: Location): Option[Cell] = currentFoodLocation match {
          case Some((foodLocation, classes)) if foodLocation == location => Some(Cell(FoodCell, x, y, snakeGameWorld.board.height, classes))
          case _ => None
        }
      }

      object InObstacle {
        def unapply(location: Location): Option[Cell] = {
          if(turnState.obstacles.contains(location)) Some(Cell(ObstacleCell, x, y, snakeGameWorld.board.height))
          else None
        }
      }

      location match {
        case InSnake(snakeCell) => snakeCell
        case InFood(foodCell) => foodCell
        case InObstacle(obstacleCell) => obstacleCell
        case _ => Cell(snakeGameWorld.board.cellAt(location), x, y, snakeGameWorld.board.height)
      }
    }

    val nodes: immutable.Seq[ElementNode] = for {
      y <- Range(0, snakeGameWorld.board.height)
      x <- Range(0, snakeGameWorld.board.width)
      cell = convertToApparantCell(x, y)
      element <- cell.render()
      node = ElementNode(element)
    } yield node

    val children = nodes.toVector
    val gameOverElementNodes: Vector[ElementNode] = GameOver(turnState.playing).render().map(elem => ElementNode(elem))
    val board: ElementNode = ElementNode(Element("div", Vector("board"), None, children))
    val container = Element("div", Vector("container"), None, board +: gameOverElementNodes)
    Vector(container)
  }
}

case class SnakeGameContainer(children: Vector[Component]) extends Component {
  def render(): Vector[Element] = {
    val elementNodes: Vector[ElementNode] = for {
      element <- children.flatMap(_.render())
    } yield ElementNode(element)
    Vector(Element("div", Vector("container"), None, elementNodes))
  }
}