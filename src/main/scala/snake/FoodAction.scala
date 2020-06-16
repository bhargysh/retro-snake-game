package snake

import cats.{Applicative, Functor, Id, Monad}
import cats.implicits._

sealed trait FoodAction

case object GrowSnake extends FoodAction
case object AddFood extends FoodAction
case object FoodReady extends FoodAction
case class MovedSnake(newSnake: Snake) extends FoodAction

sealed trait Script[F[_], A]
//TODO: Create Monad instance

case class Pure[F[_], A](a: A) extends Script[F, A]
case class Bind[F[_], A](a: F[Script[F, A]]) extends Script[F, A]

trait Interpreter[F[_], G[_]] { // F might be FoodAction and G might be IO, needs to interpret F in terms of G
  def interpret[A](fOfA: F[A]): G[A] // natural transformation => like map but on the outside not the inside
}

object Interpreter {
  def main(args: Array[String]): Unit = {
    val script: Pure[Wrap, String] = Pure("🌈") // Pure[F,A]
    val bindScript = for {
      growSnake <- FoodAction.liftF(FoodActionWrapper(GrowSnake))
      s <- script
    } yield s
    val interpreter = new Interpreter[Wrap, Id] {
      override def interpret[A](wrappedFoodAction: Wrap[A]): Id[A] = {
        wrappedFoodAction match {
          case FoodActionWrapper(foodAction) => println(foodAction)
          case FoodActionWrapper2(scriptWrapAndA) => println(???)
        }
      }
    }
    println(FoodAction.evaluate(script, interpreter))
    println("❄❄❄❄❄❄❄❄❄❄❄❄❄❄️")
//    println(FoodAction.evaluate(bindScript, interpreter))
  }
}

object FoodAction {
  def evaluate[A, F[_], G[_]: Monad](a: Script[F, A], interpreter: Interpreter[F, G]): G[A] = a match {
    case Pure(a) => Applicative[G].pure(a)
    case Bind(x) => interpreter.interpret(x).flatMap((a: Script[F, A]) => evaluate(a, interpreter))
  }
  def liftF[F[_]: Functor, A](fOfA: F[A]): Script[F, A] = Bind(fOfA.map((x: A) => Pure(x)))
}

sealed trait Wrap[T]

case class FoodActionWrapper(foodAction: FoodAction) extends Wrap[Unit]
case class FoodActionWrapper2[A, B](wrapOfA: Wrap[A], f: A => B) extends Wrap[B] // needs to be able to produce any type, consequence of being a functor as map has to work for any types A and B

object Wrap {
  implicit def functorWrap: Functor[Wrap] = new Functor[Wrap] {
    override def map[A, B](fa: Wrap[A])(f: A => B): Wrap[B] = {
      fa match {
        case FoodActionWrapper(_) => FoodActionWrapper2(fa, f)
        case FoodActionWrapper2(wrapX, xToA) => FoodActionWrapper2(wrapX, f.compose(xToA))
      }
    }
  }
}

//Why Script? Script is separate from FoodAction, it doesn't know anything about snake game logic