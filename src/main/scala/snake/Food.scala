package snake

sealed trait Food {
  def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[FoodAction]
}
case class FoodPresent(location: Location, expiryTime: MoveNumber) extends Food {
  override def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[FoodAction] = {
    if (location == snakeHead) {
      Vector(AddFood, GrowSnake)
    } else if (moveNumber == expiryTime) {
      Vector(AddFood)
    } else {
      Vector.empty
    }
  }
}
case class FoodAbsent(turns: MoveNumber) extends Food {
  override def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[FoodAction] = {
    if(turns == moveNumber) { //TODO: revisit this logic, similar to FoodPresent eat()
      Vector(FoodReady)
    } else {
      Vector.empty
    }
  }
}
