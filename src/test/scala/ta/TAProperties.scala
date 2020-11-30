package ta

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object TAPropertes extends Properties("ta") {

  // "component level tests"
  // adding states, transitions to TA are reflected in structure
  // similar is true for TCN, make sure the basic machinery is sound

  // computational correctness of transformation from TA to TCN should also be checked
  // this can be done with ScalaCheck as well
  // build TA with known condition
  // check if TCN reflects it
  // make sure check identifies inconsistencies as well as reachable times

  // "big tests" that check model checking ability
  // this should be done with ScalaCheck tool
  // testIsConsistent on TA known to be consistent (?)
  // testIsConsistent on TA known to be inconsistent (?)
  // forAllPaths query
  // pathExists query
  // other queries from timed-computation tree logic?

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a+b).length > a.length && (a+b).length > b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }

}
