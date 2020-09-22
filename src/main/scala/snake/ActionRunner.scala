package snake

import cats.Monad
import cats.implicits._
import scala.util.{Right => EitherRight}
import scala.util.{Left => EitherLeft}

class ActionRunner[A, M[_]](f: A => M[Vector[A]])(implicit M: Monad[M]) {

  def go(actions: Vector[A]): M[Unit] = {
    M.tailRecM(actions) {
      case Vector() => M.pure(EitherRight(()))
      case action +: oldActions => for {
        newActions <- f(action)
      } yield EitherLeft(oldActions ++ newActions)
    }
  }

}
