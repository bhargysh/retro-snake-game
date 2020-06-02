package snake

import cats.{Applicative, Monad}
import cats.implicits._

sealed trait FoodAction

case object GrowSnake extends FoodAction
case object AddFood extends FoodAction
case object FoodReady extends FoodAction
case class MovedSnake(newSnake: Snake) extends FoodAction

sealed trait Script[A] //TODO: fix this

case class Pure[A](a: A) extends Script[A]
case class Bind[A, B](a: Script[A], f: A => Script[B]) extends Script[B]
case class ScriptAction(foodAction: FoodAction) extends Script[Unit]

trait Interpreter[F[_]] {
  def interpret[A](foodActionOfA: FoodAction): F[A]
}


object FoodAction {
  def evaluate[A, F[_]: Monad](a: Script[A], interpreter: Interpreter[F]): F[A] = a match {
    case Pure(a) => Applicative[F].pure(a)
    case Bind(a, f) => evaluate(a, interpreter).flatMap(a => evaluate(f(a), interpreter))
    case ScriptAction(foodAction) => interpreter.interpret(foodAction) //TODO: actually implement interpret
  }
}

//script is separate from food action, doesn't know anything about game