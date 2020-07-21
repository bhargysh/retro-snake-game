package snake

case class Snake(location: List[Location], length: Int, direction: Direction) {
  def forward(): Snake = {
    val snakeHead = location.head
    val newHead = direction match {
      case Up => snakeHead.copy(y = snakeHead.y + 1)
      case Down => snakeHead.copy(y = snakeHead.y - 1)
      case Left => snakeHead.copy(x = snakeHead.x - 1)
      case Right => snakeHead.copy(x = snakeHead.x + 1)
    }
    val newSnakeLocation = newHead :: location
    this.copy(location = newSnakeLocation.take(length))
  }

  def turn(newDirection: Direction): Snake = {
    if (validateDirection(newDirection)) this.copy(direction = newDirection)
    else this
  }

  private def validateDirection(newDirection: Direction): Boolean = direction match {
    case Up | Down => newDirection == Left || newDirection == Right
    case Left | Right => newDirection == Up || newDirection == Down
  }

  def move(direction: Option[Direction]): Vector[FoodAction] = {
    val turnedSnake = direction match {
      case Some(newDirection) => turn(newDirection)
      case _ => this
    }
    val movedSnake: Snake = turnedSnake.forward()
    Vector(MovedSnake(movedSnake))
  }
}
