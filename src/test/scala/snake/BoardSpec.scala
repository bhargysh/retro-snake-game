package snake

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import Generators._
import org.scalacheck.{Arbitrary, Gen}

class BoardSpec extends Specification with ScalaCheck {
  private implicit val arbDimension: Arbitrary[Dimension] = Arbitrary {
    Gen.choose(3, 20).map(Dimension.apply)
  }
  "Board" should {
    "returns the cell at a specific location" >> prop { (location: Location) =>
      val cellArray = Array.fill[Cell](100)(Wall)
        cellArray.update(location.x + location.y * 10, FoodCell)
      val board: Board = Board(cellArray, 10, 10)
      board.cellAt(location) should beEqualTo(FoodCell)
    }
    "returns the cell index" >> prop { (location: Location) =>
      val board: Board = Board(Array.fill(100)(EmptyCell), 10, 10)
      board.cellIndex(location.x, location.y) should beBetween(0, 99)
    }
    "generates location for each cell on board" >> prop { (w: Dimension, h: Dimension) =>
      val width = w.value
      val height = h.value
      val board: Board = Board(Array.fill(width * height)(EmptyCell), width, height)
      board.locations.toVector.map { location =>
        board.cellIndex(location.x, location.y)
      }.sorted should containAllOf(Range(0, width * height)).inOrder
    }
    "return true if cell is a wall" in {
      val board = SnakeGameWorld.board
      val wallLocation = Location(9, 9)
      board.isWall(wallLocation) should beTrue
    }
    "return false if cell is not a wall" in {
      val board = SnakeGameWorld.board
      val wallLocation = Location(8, 8)
      board.isWall(wallLocation) should beFalse
    }
  }
  case class Dimension(value: Int)

}