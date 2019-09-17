package snake

import org.scalajs.dom.{Document, Node}

class SnakeGameHtml(document: Document) {
  def render(snakeGameWorld: SnakeGameWorld): Node = {
    val board = document.createElement("div")
    board.classList.add("board")
    Range(0, snakeGameWorld.board.height).map { y =>
      Range(0, snakeGameWorld.board.width).map { x =>
        val cell = document.createElement("div")
        cell.setAttribute("style", s"grid-column: $x; grid-row: $y;")
        val text = document.createTextNode(snakeGameWorld.board.cell(x + y * snakeGameWorld.board.width).toString)
        cell.appendChild(text)
        board.appendChild(cell)
      }
    }

    board
  }

//TODO: add snake onto board
  // TODO: add a boundary / wall
}
