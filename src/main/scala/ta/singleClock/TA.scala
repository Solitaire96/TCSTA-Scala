package ta.singleClock

import tcn._

object TA {
  def fromLists(states : List[State], transitions : List[Transition]) : TA = {
    val ta = new TA()

    for(state <- states) {
      ta.addState(state.label,state.inv, state.reset)
    }

    for(transition <- transitions) {
      ta.addTransition(transition.source, transition.dest, transition.guard)
    }

    ta.setInit(states.head.label)

    ta
  }
}

class TA extends ta.TA {
  var states = Map[String, State]()
  // map states to transition in adjacency list form
  var adj = Map[String, List[Transition]]()

  var init = ""

  override def toString: String = this.states.values + "\n" + this.adj.toSeq + "\n"

  // allow user to add state with reset
  def addState(label: String, inv: Option[ta.Invariant] = None, reset: Option[Int] = None): Unit = {
    val st = new State(label, inv, reset)
    this.states += (label -> st)
  }

  def addTransition(source: String, dest: String, guard: Option[ta.Invariant] = None): Unit = {
    val ts = new Transition(source, dest, guard)
    if (!this.adj.contains(source)) this.adj += source -> List()
    for (transitions <- this.adj.get(source)) this.adj += source -> (ts :: transitions)
  }

  def setInit(init: String): Unit = {
    for(state <- this.states.get(init)) {
      state.setReset(0)
      this.init = init
    }
  }

  def toTCN() = {
    val tcn = new TCN()

    // add states and abstract out invariants
    for((label, state) <- this.states.toSeq) {
      val stateBegin = label + "-b"
      val stateEnd = label + "-e"

      tcn.addEvent(stateBegin)
      tcn.addEvent(stateEnd)

      //println(state.inv)
      //println(state.inv.isDefined)

      if(state.inv.isDefined) tcn.addEdge(stateBegin, stateEnd, state.inv.get.toConstraint(), true)
      else tcn.addEdge(stateBegin, stateEnd, new Constraint(0, Constraint.inf), true)

    }

    // set initial state
    tcn.setInit(this.init + "-b")

    // add edges with guards->constraints

    for(origin <- this.adj.values; transition <- origin) {
      val source = transition.source + "-e"
      val dest = transition.dest + "-b"

      if (transition.guard.isDefined) tcn.addEdge(source, dest, transition.guard.get.toConstraint(), true)
      else tcn.addEdge(source, dest, new Constraint(0, 0), true)


    }

    // perform topological sorting of events
    val sort = tcn.topSort()

    //println(sort)

    // gather the clock resets in topological order
    var clockResets = List[(String, Int)]()
    var currentReset = (tcn.init,0)


    for(event <- sort) {
      val state = this.states(event.dropRight(2))
      if (event == state.label + "-b" && state.reset.isDefined) currentReset = (event, state.reset.get)
      clockResets = currentReset :: clockResets
    }

    clockResets = clockResets.reverse

    //println(clockResets)
    //println(clockResets.length)

    // replace constrained edges with adjusted constrained edges

    for((event, (resetState, reset)) <- sort.zip(clockResets);
        edge <- tcn.events(event);
        if (edge.constr.isDefined && !edge.constr.get.isTrivialConstraint)) {
      tcn.removeEdge(edge.source, edge.dest)
      tcn.addEdge(resetState, edge.dest, edge.constr.get.getTimeAdjustedConstraint(reset), resetState == edge.source)

      // this step needs to be added to algo description

      // check if the two
      if(resetState != edge.source) {
        if(edge.source.substring(0, edge.source.length - 2) == edge.dest.substring(0, edge.dest.length - 2))
          tcn.addEdge(edge.source, edge.dest, new Constraint(0, Constraint.inf), true)
        else
          tcn.addEdge(edge.source, edge.dest, new Constraint(0, 0), true)
      }

    }



    tcn
  }

}