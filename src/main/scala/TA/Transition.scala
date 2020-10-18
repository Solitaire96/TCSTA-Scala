package TA

import TCN._

class Transition(val source : String, val dest : String, val guard: Option[Invariant] = None) {
  // guard is an Invariant representing the guard condition
  // source is the originating state
  // dest is the destination state

  override def toString() = "(" + this.source + "->" + this.dest + ", " + this.guard + ")"
}