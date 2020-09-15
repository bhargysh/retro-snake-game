package snake

import cats.Monad
import cats.implicits._

class ActionRunner[A, M[_]](f: A => M[Vector[A]])(implicit M: Monad[M]) {

  def go(actions: Vector[A]): M[Unit] = {
    actions match {
      case Vector() => M.pure(())
      case action +: oldActions => for {
        newActions <- f(action)
        _ <- go(oldActions ++ newActions)
      } yield ()
    }
  }
//  TODO: get rid of recursion

}
