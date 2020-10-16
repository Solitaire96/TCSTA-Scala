package TCN

// Refactor this class to be immutable

class Path(val source : String, val dest : String) {
  var path = List(dest, source)

  def addNode(node : String) : Unit = {
    if (node != this.source && node != this.dest) this.path = dest :: node :: this.path.tail
  }

  override def toString() = path.reduceLeft(_+"->"+_)
}