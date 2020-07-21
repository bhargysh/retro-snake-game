package snake

case class Board(cell: Array[Cell], width: Int, height: Int) {
  def cellAt(location: Location): Cell = {
    cell(cellIndex(location.x, location.y))
  }
  def cellIndex(x: Int, y: Int): Int = {
    x + y * width
  }
  def locations: Set[Location] = {
    Range(0, height).flatMap { y =>
      Range(0, width).map { x =>
        Location(x, y)
      }
    }.toSet
  }
  def isWall(location: Location): Boolean = {
    this.cellAt(location) == Wall
  }
}
