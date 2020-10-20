package snake.components

import org.specs2.mutable.Specification

class SnakeGameContainerSpec extends Specification {
  "Snake Game Container" should {
    "render" in {
      def containerDiv(element: Element) = {
        element.tag shouldEqual "div"
        element.classes should contain(exactly("container"))
        element.style shouldEqual None
        element.children should ??? //TODO
      }

      SnakeGameContainer(Board(), GameOver(isPlaying = true)).render() should contain(exactly(containerDiv _))
    }
  }
}
