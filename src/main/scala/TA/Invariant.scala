package TA

import TCN.Constraint

class Invariant(clock: String, rel: String, const: Double) {
  // clock is a string with the variable name of the clock, e.g. "x"
  // rel is a string representing the relation itself, e.g. "<="
  // const is a double representing the bound of the invariant, e.g. 7 in "x <= 7"

  override def toString: String = this.clock + this.rel + this.const

  def toConstraint(): Unit = rel match {
    case "<" => new Constraint(0, const - Constraint.epsilon)
    case "<=" => new Constraint(0, const)
    case ">" => new Constraint(const + Constraint.epsilon, Constraint.inf)
    case ">=" => new Constraint(const, Constraint.inf)
  }

}