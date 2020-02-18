package snake
import org.scalajs.dom
import dom.{Node, document, window}
import org.scalajs.dom.raw.{Element, KeyboardEvent}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Main")
object Main {
  @JSExport
  def main(args: Array[String]): Unit = {
    appendPar(document.body, "Snake game!!!!")
    val world = SnakeGameWorld.newSnakeGameWorld
    val html = new SnakeGameHtml(document)
    val renderedWorld = html.render(world)
    val boardUI = document.createElement("div")
    window.setTimeout(() => updateGame(world, html, renderedWorld, boardUI), 1000)
    document.body.appendChild(boardUI)
    boardUI.appendChild(renderedWorld)
    document.addEventListener("keydown", (event: KeyboardEvent) => {
      val maybeDirection = Keyboard.stringToDirection(event.key)
      maybeDirection match {
        case Some(value) => boardUI.setAttribute("data-direction", value.toString)
        case None => None
      }
    })
  }

  private def updateGame(world: SnakeGameWorld, html: SnakeGameHtml, oldWorld: Node, boardUI: Element): Unit = {
    val maybeDirectionData: Option[String] = Option(boardUI.getAttribute("data-direction"))
    val maybeDirection = maybeDirectionData.flatMap(Direction.fromStr)
    val newWorld = world.play(maybeDirection)
    val newRenderedWorld = html.render(newWorld)
    boardUI.replaceChild(newRenderedWorld, oldWorld)
    if (newWorld.isPlaying) {
      window.setTimeout(() => updateGame(newWorld, html, newRenderedWorld, boardUI), 1000)
    }
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
}
//TODO: tests
//TODO: animation
//TODO: game over rendering
