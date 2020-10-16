package TA

import TCN._

class Transition(source : String, dest : String, guard: Option[Invariant] = None) {
  // guard is an Invariant representing the guard condition
  // source is the originating state
  // dest is the destination state

  override def toString() = "(" + this.source + "->" + this.dest + ", " + this.guard + ")"
}