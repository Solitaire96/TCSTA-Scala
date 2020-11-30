package ta.singleClock

import ta.Run

class SimulateTA(val ta: TA, val run : Run) {


  def simulate(): Boolean = {
    var time = 0
    var clock = 0
    var currentState = ta.init

    var result = true

    for(step <- run) {
      //println("entering location " + step._1)
      //println("time is " + time)
      //println("clock x is " + clock)
      for(inv <- ta.states(step._1).inv) {
        val constr = inv.toConstraint()
        //println("constraint is " + constr)
        val sat = clock >= constr.lower && clock <= constr.upper
        //println("satisfied or not " + sat)
        if(!sat) result = false
      }
      time += step._2
      clock += step._2
      //println("spending delta t = " + step._2 + " in location " + step._1)
      for(inv <- ta.states(step._1).inv) {
        val constr = inv.toConstraint()
        //println("constraint is " + constr)
        val sat = clock >= constr.lower && clock <= constr.upper
        //println("satisfied or not " + sat)
        if(!sat) result = false
      }
      for(transition <- ta.adj(step._1).find(transition => transition.dest == step._3); guard <- transition.guard) {
        val constr = guard.toConstraint()
        //println("guard is " + constr)
        val sat = clock >= constr.lower && clock <= constr.upper
        if(!sat) result = false
      }
      //println("exiting location " + step._1 + " and entering location " + step._3)
      //println("time is " + time)
      //println("clock x is " + clock)
      for(reset <- ta.states(step._3).reset) clock = reset
      currentState = step._3
    }

    // make sure the final state's invariant holds
    for(inv <- ta.states(currentState).inv) {
      val constr = inv.toConstraint()
      //println("constraint is " + constr)
      val sat = clock >= constr.lower && clock <= constr.upper
      //println("satisfied or not " + sat)
      if(!sat) result = false
    }



    result
  }

}
