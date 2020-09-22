package snake

import org.specs2.mutable.Specification
import cats.Id
import cats.data.Writer
import cats.implicits._

class ActionRunnerSpec extends Specification {

  "Action Runner" should {
    type TestWriter[A] = Writer[Vector[Int], A]
    "return unit when actions is an empty vector" in {
      new ActionRunner[Int, Id](_ => ???).go(Vector.empty) shouldEqual(())
    }

    "run actions when actions is not empty" in {
      val actions = Vector(1, 2, 3)
      val actionRunner = new ActionRunner[Int, TestWriter](n => Writer.apply(Vector(n), Vector.empty))
      actionRunner.go(actions).run shouldEqual((actions, ()))
    }

    "return new actions" in {
      val action = Vector(20000)
      val actionRunner = new ActionRunner[Int, TestWriter](n => {
        if(n > 0) Writer.apply(Vector(n), Vector(n - 1))
        else Writer.apply(Vector(n), Vector.empty)
      })

      actionRunner.go(action).run shouldEqual((Vector.range(20000, -1, -1), ()))
    }
  }

}
