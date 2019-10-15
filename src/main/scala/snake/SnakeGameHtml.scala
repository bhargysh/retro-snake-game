package snake

import org.scalajs.dom.{Document, Node}

class SnakeGameHtml(document: Document) {
  def render(snakeGameWorld: SnakeGameWorld): Node = {
    val board = document.createElement("div")
    board.classList.add("board")
    val newSnakeGameWorld = putSnakeOn(snakeGameWorld)
    Range(0, snakeGameWorld.board.height).map { y =>
      Range(0, snakeGameWorld.board.width).map { x =>
        val cell = document.createElement("div")
        cell.setAttribute("style", s"grid-column: ${x+1}; grid-row: ${snakeGameWorld.board.height - y};")
        val text = document.createTextNode(
          cellEmoji(newSnakeGameWorld.board.cellAt(Location(x, y)))
        )
        cell.appendChild(text)
        board.appendChild(cell)
      }
    }

    board
  }

  def putSnakeOn(snakeGameWorld: SnakeGameWorld): SnakeGameWorld = {
    val currentSnakeLocation = snakeGameWorld.snake.location.map {
      case Location(x, y) => snakeGameWorld.board.cellIndex(x, y)
    }
    val newCells = snakeGameWorld.board.cell.zipWithIndex.map{
      case (cell, index) => if(currentSnakeLocation.contains(index)) SnakePart else cell
    }
    snakeGameWorld.copy(board = snakeGameWorld.board.copy(cell = newCells))

  }

  def cellEmoji(cell: Cell): String = cell match {
    case SnakePart => "🐍"
    case Wall => "🛑"
    case EmptyCell => " "
  }

}