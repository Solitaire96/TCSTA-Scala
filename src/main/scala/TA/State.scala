package TA

class State(var label: String, var inv: Option[Invariant] = None, var reset: Option[Integer] = None) {
  // inv is an Invariant representing the invariant condition

  def setReset(reset: Integer) = this.reset = Some(reset)

  override def toString: String = {
    val str = "(%s%s%s)"
    (inv, reset) match {
      case (Some(i), Some(r)) => str.format(this.label, ", " + i, ", reset=" + r)
      case (Some(i), None) => str.format(this.label, ", " + i, "")
      case (None, Some(r)) => str.format(this.label, "", ", reset=" + r)
      case _ => str.format(this.label, "", "")
    }
  }

}
