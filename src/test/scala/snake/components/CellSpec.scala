package snake.components

import org.specs2.mutable.Specification

class CellSpec extends Specification {

  "Cell" should {
    "render a cell component" in {
      val style = s"grid-column: 1; grid-row: 5;"
      Cell(snake.SnakePart, 0, 5, 10).render() shouldEqual
        Vector(Element("div", Vector.empty, Some(style), Vector(TextNode("🐍"))))
    }
  }

}
