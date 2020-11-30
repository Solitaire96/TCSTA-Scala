package tcn

class Constraint(val lower : Double = 0, val upper : Double = Double.PositiveInfinity) {
  def isTrivialConstraint = this.lower == 0 && (this.upper == 0 || this.upper.isInfinity)

  def getTimeAdjustedConstraint(time: Double) = {
    val l = math.max(0, this.lower - time)
    val u = math.max(0, this.upper - time)

    new Constraint(l, u)
  }

  def intersect(other : Constraint) : Option[Constraint] = {
    val lower = math.max(this.lower, other.lower)
    val upper = math.min(this.upper, other.upper)
    if (lower <= upper) Some(new Constraint(lower, upper))
    else None
  }

  def compose(other : Constraint) = new Constraint(this.lower + other.lower, this.upper + other.upper)

  override def toString() = "[" + this.lower + "," + this.upper + "]"

}

object Constraint {
  val epsilon = 1e-9
  val inf = Double.PositiveInfinity
}