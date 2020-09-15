package snake

import org.specs2.mutable.Specification
import cats.Id
import cats.data.Writer
import cats.implicits._

class ActionRunnerSpec extends Specification {

  "Action Runner" should {
    "return unit when actions is an empty vector" in {
      new ActionRunner[Int, Id](n => ???).go(Vector.empty) shouldEqual(())
    }

    "run actions when actions is not empty" in {
      val actions = Vector(1, 2, 3)
      type TestWriter[A] = Writer[Vector[Int], A]
      val actionRunner = new ActionRunner[Int, TestWriter](n => Writer.apply(Vector(n), Vector.empty))
      actionRunner.go(actions).run shouldEqual((actions, ()))
    }
  }
//  TODO: test for running new actions

}
