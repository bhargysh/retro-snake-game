package snake

sealed trait Food
case class FoodPresent(location: Location, expiryTime: MoveNumber) extends Food
case class FoodAbsent(turns: MoveNumber) extends Food
