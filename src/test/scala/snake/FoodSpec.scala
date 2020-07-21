package snake

import org.specs2.mutable.Specification

class FoodSpec extends Specification {
  "eat" should {
    "return food action if snake head is at food location" in {
      val food = FoodPresent(Location(3,3), MoveNumber(5))
      food.eat(Location(3,3), MoveNumber(3)) mustEqual Vector(AddFood, GrowSnake)
    }
    "return food action if move number is same as expiry time" in {
      val food = FoodPresent(Location(3,3), MoveNumber(5))
      food.eat(Location(3,2), MoveNumber(5)) mustEqual Vector(AddFood)
    }
    "return no food action if snake not at location and expiry time is ongoing" in {
      val food = FoodPresent(Location(3,3), MoveNumber(5))
      food.eat(Location(3,2), MoveNumber(4)) mustEqual Vector.empty
    }
    "return food action if turns is same as move number" in {
      val food = FoodAbsent(MoveNumber(4))
      food.eat(Location(3,2), MoveNumber(4)) mustEqual Vector(FoodReady)
    }
    "return no food action if turns is not same as move number" in {
      val food = FoodAbsent(MoveNumber(5))
      food.eat(Location(3,2), MoveNumber(4)) mustEqual Vector.empty
    }
  }
}
