package snake

import org.specs2.mutable.Specification

class FoodSpec extends Specification {
  "eat" should {
    "return food eaten if snake head is at food location" in {
      val food = FoodPresent(Location(3,3), 5)
      food.eat(Location(3,3), 3) mustEqual FoodEaten
    }
    "return food expired if move number is same as expiry time" in {
      val food = FoodPresent(Location(3,3), 5)
      food.eat(Location(3,2), 5) mustEqual FoodExpired
    }
    "return food not eaten if snake not at location and expiry time is ongoing" in {
      val food = FoodPresent(Location(3,3), 5)
      food.eat(Location(3,2), 4) mustEqual FoodNotEaten
    }
    "return food added if turns is same as move number" in {
      val food = FoodAbsent(4)
      food.eat(Location(3,2), 4) mustEqual FoodAdded
    }
    "return food not eaten if turns is not same as move number" in {
      val food = FoodAbsent(5)
      food.eat(Location(3,2), 4) mustEqual FoodNotEaten
    }
  }
}
