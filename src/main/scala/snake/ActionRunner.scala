package snake

import cats.Monad
import cats.implicits._
import scala.util.{Right => EitherRight}
import scala.util.{Left => EitherLeft}

trait ActionRunner[F[_], A] {
  def runActions(actions: Vector[A]): F[Unit]
}


class ActionRunnerImpl[F[_], A](f: A => F[Vector[A]])(implicit M: Monad[F]) extends ActionRunner[F, A] {

  def runActions(actions: Vector[A]): F[Unit] = {
    M.tailRecM(actions) {
      case Vector() => M.pure(EitherRight(()))
      case action +: oldActions => for {
        newActions <- f(action)
      } yield EitherLeft(oldActions ++ newActions)
    }
  }

}
