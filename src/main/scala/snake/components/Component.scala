package snake.components

import snake.{EmptyCell, FoodCell, SnakePart, Wall}

// <SnakeGameContainer>
//    <Board>
//      <Cell>
//    <GameOver>

sealed trait Node
case class TextNode(data: String) extends Node
case class ElementNode(data: Element) extends Node

case class Element(tag: String, classes: Vector[String], style: Option[String], children: Vector[Node])

trait Component {

  def render(): Vector[Element]
}

case class Cell(cell: snake.Cell, x: Int, y: Int, boardHeight: Int) extends Component {
  def render(): Vector[Element] = {
    val text = cell match {
      case SnakePart => "🐍"
      case Wall => "🛑"
      case EmptyCell => " "
      case FoodCell => "🍕"
    }
    Vector(
      Element("div",
      Vector.empty,
      Some(s"grid-column: ${x+1}; grid-row: ${boardHeight - y};"),
      Vector(TextNode(text))))
  }
}

//TODO: create other components as mentioned above