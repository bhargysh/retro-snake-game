package snake

import cats.{Applicative, Functor, Id, Monad}
import cats.implicits._

sealed trait FoodAction

case object GrowSnake extends FoodAction
case object AddFood extends FoodAction
case object FoodReady extends FoodAction
case class MovedSnake(newSnake: Snake) extends FoodAction

sealed trait Script[F[_], A]

case class Pure[F[_], A](a: A) extends Script[F, A]
case class Bind[F[_], A](a: F[Script[F, A]]) extends Script[F, A]

object Script {
//  type SF[F[_]] = ({ type L[A] = Script[F, A] })#L //script takes [_,_] and monad takes [_]

  implicit def monadScript[F[_]: Functor]: Monad[({ type f[x] = Script[F, x] })#f] = new Monad[({ type f[x] = Script[F, x] })#f] {
    override def pure[A](x: A): Script[F,A] = Pure(x)

    override def flatMap[A, B](fa: Script[F,A])(f: A => Script[F,B]): Script[F,B] = fa match {
      case Bind(y) => Bind(y.map((sf: Script[F, A]) => flatMap(sf)(f))) // F[Script[F, B]] => Script[F, B]
      case Pure(x) => f(x)
    }
//TODO: this
    override def tailRecM[A, B](a: A)(f: A => Script[F, Either[A, B]]): Script[F,B] = ???
  }
}


trait Interpreter[F[_], G[_]] { // F might be FoodAction and G might be IO, needs to interpret F in terms of G
  def interpret[A](fOfA: F[A]): G[A] // natural transformation => like map but on the outside not the inside
}

object Interpreter {
  def main(args: Array[String]): Unit = {
    type SF[A] = Script[Wrap, A]
    implicit val monadWTF: Monad[SF] = Script.monadScript
    val script: Pure[Wrap, String] = Pure("🌈") // Pure[F,A]
    val bindScript = monadWTF.flatMap(FoodAction.liftF(FoodActionWrapper(GrowSnake)))(_ => monadWTF.map(script)(s => s))

    val interpreter = new Interpreter[Wrap, Id] {
      override def interpret[A](wrappedFoodAction: Wrap[A]): Id[A] = {
        wrappedFoodAction match {
          case FoodActionWrapper(foodAction) => println(foodAction)
          case FoodActionWrapper2(wrapSomething, func) => func(interpret(wrapSomething))
        }
      }
    }
    println(FoodAction.evaluate(script, interpreter))
    println("❄❄❄❄❄❄❄❄❄❄❄❄❄❄️")
    println(FoodAction.evaluate(bindScript, interpreter))
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