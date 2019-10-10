package snake

object Keyboard {
  def stringToDirection(keyboardInput: String): Option[Direction] = keyboardInput match {
    case "ArrowUp" | "w" => Some(Up)
    case "ArrowDown" | "s" => Some(Down)
    case "ArrowLeft" | "a" => Some(Left)
    case "ArrowRight" | "d" => Some(Right)
    case _ => None
  }
}
