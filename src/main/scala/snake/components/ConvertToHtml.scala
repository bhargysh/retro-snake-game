package snake.components

import org.scalajs.dom.raw.{Document, Element => DOMElement}

object ConvertToHtml {
  def convert(element: Element, document: Document): DOMElement = {
    val result = document.createElement(element.tag)
    element.style.foreach(s => result.setAttribute("style", s))
    element.classes.foreach(c => result.classList.add(c))
    element.children.foreach(ch => result.appendChild(ch match {
      case TextNode(data) => document.createTextNode(data)
      case ElementNode(data) => convert(data, document)
    }))
    result
  }
}
