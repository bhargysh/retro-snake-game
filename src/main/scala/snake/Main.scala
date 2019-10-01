package snake
import org.scalajs.dom
import dom.{Node, document, window}
import org.scalajs.dom.raw.Element


object Main {
  def main(args: Array[String]): Unit = {
    appendPar(document.body, "Snake game!!!!")
    val world = SnakeGameWorld.newSnakeGameWorld
    val html = new SnakeGameHtml(document)
    val renderedWorld = html.render(world)
    val boardUI = document.createElement("div")
    window.setTimeout(() => updateGame(world, html, renderedWorld, boardUI), 1000)
    document.body.appendChild(boardUI)
    boardUI.appendChild(renderedWorld)
  }

  private def updateGame(world: SnakeGameWorld, html: SnakeGameHtml, oldWorld: Node, boardUI: Element): Unit = {
    val newWorld = world.play()
    val newRenderedWorld = html.render(newWorld)
    boardUI.replaceChild(newRenderedWorld, oldWorld)
    window.setTimeout(() => updateGame(newWorld, html, newRenderedWorld, boardUI), 1000)
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
}
//TODO: collision with wall? user input for moving snake