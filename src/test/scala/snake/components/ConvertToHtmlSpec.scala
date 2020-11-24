package snake.components

import org.scalajs.dom.document
import org.scalajs.dom.raw.{Element => DOMElement}
import org.specs2.mutable.Specification

class ConvertToHtmlSpec extends Specification {

  "convert" should {
    "convert Element to a DOMElement with div tag" in {
      val element = Element("div", Vector.empty, None, Vector.empty)
      ConvertToHtml.convert(element, document) should((domElement: DOMElement) => domElement.tagName shouldEqual "DIV")
    }
    "convert Element to a DOMElement with classes" in {
      val element = Element("div", Vector("abcd", "efgh"), None, Vector.empty)
      ConvertToHtml.convert(element, document) should { (domElement: DOMElement) =>
        domElement.classList.contains("abcd") should beTrue
        domElement.classList.contains("efgh") should beTrue
      }
    }
    "convert Element to a DOMElement with styles" in {
      val element = Element("div", Vector.empty, Some("blah"), Vector.empty)
      ConvertToHtml.convert(element, document) should { (domElement: DOMElement) =>
        domElement.getAttribute("style") shouldEqual "blah"
      }
    }
    "convert Element to a DOMElement with children" in {
      val childElement = Element("span", Vector.empty, None, Vector.empty)
      val element = Element("div", Vector.empty, None, Vector(TextNode("some cool text"), ElementNode(childElement)))
      ConvertToHtml.convert(element, document) should { (domElement: DOMElement) =>
        domElement.childNodes.item(0).nodeValue shouldEqual "some cool text"
        domElement.childNodes.item(1).nodeName shouldEqual "SPAN"
      }
    }
  }

}
