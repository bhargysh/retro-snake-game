package snake
import org.scalajs.dom
import dom.document

object Main {
  def main(args: Array[String]): Unit = {
    appendPar(document.body, "Snake game!!!!")
    val world = SnakeGameWorld.newSnakeGameWorld
    val html = new SnakeGameHtml(document)
    val renderedWorld = html.render(world)
    val boardUI = document.createElement("div")
    document.body.appendChild(boardUI)
    boardUI.appendChild(renderedWorld)
  }
  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
}
