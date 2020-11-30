package tcn

class Edge(val source : String, val dest : String, val constr : Option[Constraint]  = None, val isPrimitive : Boolean = false) {
  override def toString = "(" + this.source + "->" + this.dest + ", " + this.constr + ")"
}
