package snake

import org.specs2.mutable.Specification

class KeyboardSpec extends Specification {
  "Keyboard" should {
    "return Up" in {
      "when key is w" in {
      Keyboard.stringToDirection("w") should beSome[Direction](Up)
      }
      "when key is ArrowUp" in {
        Keyboard.stringToDirection("ArrowUp") should beSome[Direction](Up)
      }
    }
    "return Down" in {
      "when key is s" in {
        Keyboard.stringToDirection("s") should beSome[Direction](Down)
      }
      "when key is ArrowDown" in {
        Keyboard.stringToDirection("ArrowDown") should beSome[Direction](Down)
      }
    }
    "return Left" in {
      "when key is a" in {
        Keyboard.stringToDirection("a") should beSome[Direction](Left)
      }
      "when key is ArrowLeft" in {
        Keyboard.stringToDirection("ArrowLeft") should beSome[Direction](Left)
      }
    }
    "return Right" in {
      "when key is d" in {
        Keyboard.stringToDirection("d") should beSome[Direction](Right)
      }
      "when key is ArrowRight" in {
        Keyboard.stringToDirection("ArrowRight") should beSome[Direction](Right)
      }
    }
    "return None if it cannot resolve a direction" in {
      Keyboard.stringToDirection("hello") should beNone
    }
  }

}
