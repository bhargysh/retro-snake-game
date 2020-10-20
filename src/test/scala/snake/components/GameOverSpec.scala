package snake.components

import org.specs2.mutable.Specification

class GameOverSpec extends Specification {
  "GameOver component" should {
    "render game over when isPlaying is false" in {
      def resultSpan(element: Element) = {
        element.tag shouldEqual "span"
        element.classes should contain(exactly("gameover-span"))
        element.style shouldEqual None
        element.children should contain(exactly[Node](TextNode("GAME OVER")))
      }

      def resultDiv(element: Element) = {
        element.tag shouldEqual "div"
        element.classes should contain(exactly("gameover"))
        element.style shouldEqual None
        element.children should contain(beLike[Node] {
          case ElementNode(elem) => resultSpan(elem)
        })
      }

      GameOver(isPlaying = false).render() should contain(exactly(resultDiv _))
    }

    "not render game over when isPlaying is true" in {
      GameOver(isPlaying = true).render() shouldEqual Vector.empty
    }
  }

}
