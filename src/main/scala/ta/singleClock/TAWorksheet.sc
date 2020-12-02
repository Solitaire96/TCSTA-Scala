

/**

val locs = List("a", "b", "c", "d", "e")
var deltas = List(2, 1, 4, 6)

 **/
type Run = List[(String, Int, String)]

def createRun(locations : List[String], deltas : List[Int]) : Run = {
  locations.zip(deltas).zip(locations.tail).map(x => (x._1._1, x._1._2, x._2))
}


//createRun(locs, deltas)

import ta.singleClock._

/*
val ta = new TA()
ta.setInit("a")
ta.addState("a", Some(new Invariant("x", "<", 15)))
ta.addState("b", Some(new Invariant("x", "<", 7)))
ta.states("b").setReset(0)
ta.addState("c", None)
ta.addTransition("a", "b", None)
ta.addTransition("b", "c", None)

val sim = new SimulateTA(ta, createRun(List("a", "b", "c"), List(10, 3)))
*/

// to do
// "randomly" generate TA
// generate a path from init to some end state
// specifically -> generate transition system / tree
// if each node of tree has a proper time, language is non-empty

import org.scalacheck.Gen




/*
val transGen = for(states <- stateGen;
                   constr <- constrGen ;
                   source <- states;
                   elem <- List("a", "b", "c", "c")
) yield(new Transition(source.label, elem, constr))
*/

/*
val transGen = for(states <- stateGen;
  source <- states;
    //dest <- states.map(x => x.label).withFilter(_ > source.label);
    constr <- constrGen;
    invar <- Gen.oneOf(None, constr);
    dest <- states.map(x => x.label).withFilter(_ > source.label)
) yield(new Transition(source.label, dest, invar))
*/





//{
    //ta.addState(loc, invar)
  //}
  //yield(new State(loc.toString, invar, None))

val numTrials = 5000
val taSize = 6
var counter = 0

var propertyHolds = true

// next step -> add automata with multiple paths
// research questions:
// "diamond structure"
// - move reset to transition instead of state
// how to handle invariants?
// can you enter a state where the invariant is false upon entering
// this drastically simplifies multi-clock case

val constrGen : Gen[Option[Invariant]] = for( pred <- Gen.oneOf("<", "<=", ">", ">=");
                                              const <- Gen.choose(1, 20))
  yield Some(new Invariant("x", pred, const))

for(i <- 1 to numTrials) {
  val alphabet = ('a' to 'z').toList.toIterator

  val stateGen = for(constr <- constrGen;
                     invar <- Gen.oneOf(None, constr);
                     resetVal  <- Gen.choose(2, 8);
                     reset <- Gen.oneOf(None, None, Some(resetVal))
                     )
    yield(new State(alphabet.next().toString, invar, reset))

  val states = List.tabulate(taSize)(_ => stateGen.sample.get).sortWith( (a,b) => a.label < b.label)

  val genTransitions = (source : String, dest : String) => for(
    constr <- constrGen;
    invar <- Gen.oneOf(None, constr, constr)
  ) yield (new Transition(source, dest, invar))


  val labels = states.map(_.label)
  val transitions = labels.zip(labels.tail).map(x => genTransitions(x._1, x._2).sample.get)
  val ta = TA.fromLists(states, transitions)



  var solution : Option[Run] = None

  for(i <- 1 to 10000) {
    val deltas = List.tabulate(taSize - 1)(_ => Gen.choose(1, 10).sample.get)
    val run = createRun(labels, deltas)
    val sim = new SimulateTA(ta, run)
    val result = sim.simulate()
    if (result) {
      solution = Some(run)
    }
  }

  for(run <- solution) {
    //println("found a run!")
    //println(run)

    val tcn = ta.toTCN()
    propertyHolds &= tcn.isConsistent()
    counter += 1
    //println(tcn)
    //println("TCN is consistent: ")
  }
}

println("property is " + propertyHolds + " across " + counter + " ta")



/*
val ta = new TA()
ta.addState("a", Some(new Invariant("x", "<", 15)))
ta.addState("b", Some(new Invariant("x", "<", 7)))
ta.states("b").setReset(4)
ta.addState("c", Some(new Invariant("x", "<", 10)))
ta.addTransition("a", "b", Some(new Invariant("x", ">", 5)))
ta.addTransition("b", "c", None)
ta.setInit("a")

println(ta)

val tcn = ta.toTCN()
println(tcn)
println("TCN is consistent: " + tcn.isConsistent())
*/




