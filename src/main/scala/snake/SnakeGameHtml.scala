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
          newSnakeGameWorld.board.cell(cellIndex(newSnakeGameWorld, x, y)).toString
        )
        cell.appendChild(text)
        board.appendChild(cell)
      }
    }

    board
  }

  private def cellIndex(snakeGameWorld: SnakeGameWorld, x: Int, y: Int) = {
    x + y * snakeGameWorld.board.width
  }

  def putSnakeOn(snakeGameWorld: SnakeGameWorld): SnakeGameWorld = {
    val currentSnakeLocation = snakeGameWorld.snake.location.map {
      case Location(x, y) => cellIndex(snakeGameWorld, x, y)
    }
    val newCells = snakeGameWorld.board.cell.zipWithIndex.map{
      case (cell, index) => if(currentSnakeLocation.contains(index)) SnakePart else cell
    }
    snakeGameWorld.copy(board = snakeGameWorld.board.copy(cell = newCells))

  }
}
//TODO: make the snake move, read from the keys
