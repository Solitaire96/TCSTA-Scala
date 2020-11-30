import tcn._

package object ta {
  type Run = List[(String, Int, String)]

  object Run {
    def createRun(locations : List[String], deltas : List[Int]) : Run = {
      locations.zip(deltas).zip(locations.tail).map(x => (x._1._1, x._1._2, x._2))
    }
  }

  trait Invariant {
    def toConstraint(): Constraint
  }

  trait State {}

  trait Transition {}

  trait TA {
    def addState(label: String, inv: Option[Invariant], reset: Option[Int]): Unit

    def addTransition(source: String, dest: String, guard: Option[Invariant]): Unit

    def setInit(init: String): Unit

    def toTCN(): TCN

  }
}
