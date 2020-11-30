package ta.singleClock

import ta.Run

class SimulateTA(val ta: TA, val run : Run) {
  var time = 0
  var clock = 0
  var currentState = ta.init

  def simulate(): Boolean = {
    var result = true

    for(step <- run) {
      println("entering location " + step._1)
      println("time is " + time)
      println("clock x is " + clock)
      time += step._2
      clock += step._2
      println("spending delta t = " + step._2 + " in location " + step._1)
      for(inv <- ta.states(step._1).inv) {
        val constr = inv.toConstraint()
        val sat = clock >= constr.lower && clock <= constr.upper
        if(!sat) result = step._2 == 0
      }
      for(transition <- ta.adj(step._1).find(transition => transition.dest == step._3); guard <- transition.guard) {
        val constr = guard.toConstraint()
        val sat = clock >= constr.lower && clock <= constr.upper
        if(!sat) result = false
      }
      println("exiting location " + step._1 + " and entering location " + step._3)
      println("time is " + time)
      println("clock x is " + clock)
      for(reset <- ta.states(step._3).reset) clock = reset
    }

    result
  }

}
