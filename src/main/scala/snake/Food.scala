package snake

sealed trait Food {
  def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[BoardAction]
}
case class FoodPresent(location: Location, expiryTime: MoveNumber) extends Food {
  override def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[BoardAction] = {
    if (location == snakeHead) {
      Vector(AddFood, GrowSnake)
    } else if (moveNumber == expiryTime) {
      Vector(AddFood, AddObstacle)
    } else {
      Vector.empty
    }
  }
}
case class FoodAbsent(turns: MoveNumber) extends Food {
  override def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[BoardAction] = {
    if(turns == moveNumber) {
      Vector(FoodReady)
    } else {
      Vector.empty
    }
  }
}
