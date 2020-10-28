package snake.components

import org.specs2.mutable.Specification

class SnakeGameContainerSpec extends Specification {
  "Snake Game Container" should {
    "render" in {
      def containerDiv(element: Element) = {
        element.tag shouldEqual "div"
        element.classes should contain(exactly("container"))
        element.style shouldEqual None
        element.children should beEmpty
      }
      SnakeGameContainer(Vector.empty[Component]).render() should contain(exactly(containerDiv _))
    }
    "renders its children" in {
      val elem1 = Element("div", Vector("elem1"), None, Vector.empty)
      val elem2 = Element("div", Vector("elem2"), None, Vector.empty)
      val elem3 = Element("div", Vector("elem3"), None, Vector.empty)
      def containerDiv(element: Element) = {
        element.tag shouldEqual "div"
        element.classes should contain(exactly("container"))
        element.style shouldEqual None
        element.children should contain(exactly[Node](ElementNode(elem1), ElementNode(elem2), ElementNode(elem3)))
      }
      val children = Vector[Component](() => Vector(elem1, elem2), () => Vector(elem3))
      SnakeGameContainer(children).render() should contain(exactly(containerDiv _))
    }
  }
}
