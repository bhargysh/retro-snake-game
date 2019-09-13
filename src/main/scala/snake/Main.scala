package snake
import org.scalajs.dom
import dom.document

object Main {
  def main(args: Array[String]): Unit = {
//    println("Snake game!")
//    println(document)
    appendPar(document.body, "Snake game!!!!")
  }
  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
}
