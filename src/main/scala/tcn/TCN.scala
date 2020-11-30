package tcn

import scala.collection.mutable._

class TCN {
  var events = Map[String, List[Edge]]()
  var next = Map[(String, String), String]()

  var init = ""

  val inf = Double.PositiveInfinity

  def setInit(init : String): Unit = {
    this.init = init
  }

  override def toString() = events.iterator.foldLeft("")((acc, events)
  => acc + events._1 + "-" + events._2.foldLeft("")((acc, edge) => acc + edge) + "\n")

  def addEvent(label: String): Unit = this.events += (label -> List())

  // does not verify existence of event before adding edge
  def addEdge(source: String, dest: String, constr: Constraint, isPrimitive: Boolean = false) = {
    val edge = new Edge(source, dest, Some(constr), isPrimitive)
    val edges = edge :: this.events(source)
    this.events += (source -> edges)
  }

  // does not verify existence of event before removing edge
  def removeEdge(source: String, dest: String) = {
    val edges = this.events(source)

    this.events += (source -> edges.filterNot(edge => edge.dest == dest))
  }

  def getEdge(source : String, dest: String): Option[Edge] = {
    val edges = this.events(source)

    // is known that edges has either 1 or 0 elements
    if (!edges.exists(_.dest == dest)) None else Some(edges.head)

  }

  def topSort(): List[String] = {
    def visit(event: String, visited: Buffer[String]): Unit = {
      if (!visited.contains(event)) {
        for (e <- this.events(event)) visit(e.dest, visited)
        visited.append(event)
      }
    }

    val visited = ArrayBuffer[String]()

    for (event <- this.events.keys) visit(event, visited)

    visited.reverse.toList
  }

  def topGreater(event1: String, event2: String) = {
    val sort = this.topSort()

    sort.indexOf(event1) <= sort.indexOf(event2)
  }

  def getPath(source: String, dest: String) = {
    val path = new Path(source, dest)

    var n = source

    while (n != dest) {
      path.addNode(n)
      n = this.next((n, dest))
    }

    path
  }

  def convertToTAPath(source: String, dest: String): Unit = {
    // for source in self.events.keys():
    //for dest in self.events.keys():
    //paths[(source, dest)] = self.getFullPath(source, dest)

    //return None

  }


  def findMinimalDistances() = {
  var nex = Map[(String, String), String]()

  var dist = Map[(String, String), Double]()

  // initialize the distance matrix

    for(source <- this.events.keys; dest <- this.events.keys) {
      if (source == dest) dist += (source, dest) -> 0
      else dist += (source, dest) -> inf

      nex += (source,dest) -> dest
    }

    // add the weighted distances from the TCN
    for(source <- this.events.keys; edge <- this.events(source)) {
      val dest = edge.dest
      val lower = edge.constr.get.lower
      val upper = edge.constr.get.upper

      dist += (source, dest) -> upper
      dist += (dest, source) -> -lower
    }

    var failure = false

    // Floyd-Warshall algorithm
    for(mid <- this.events.keys; source <- this.events.keys; dest <- this.events.keys) {
      val triangle = dist((source, mid)) + dist((mid, dest))
      if(triangle < dist((source, dest))) {
        dist += (source, dest) -> triangle
        nex += (source, dest) -> nex((source, mid))
      }
      // check for negative cycles
      // need to add negative cycle return in case of failure
      if (source == dest && dist((source, dest)) < 0) failure = true
    }

    //dist.keys.filter(tuple => tuple._1 == tuple._2)

    if(!failure) {
      this.next = nex
      Some(dist)
    }
    else None
}

  def isConsistent() = this.findMinimalDistances() match {
    case Some(_) => true
    case None => false
  }

  def findMinimalNetwork() = this.findMinimalDistances() match {
      case None => None
      case Some(dist) => {
        val tcn = new TCN()

        for(event <- this.events.keys) tcn.addEvent(event)
        for(event1 <- this.events.keys; event2 <- this.events.keys) {
          //if (tcn.getEdge(event1, event2) != None && tcn.getEdge(event2, event1) != None) {
            val dist12 = dist((event1, event2))
            val dist21 = dist((event2, event1))

            val lower = -math.min(dist12, dist21)
            val upper = math.max(dist12, dist21)

            if (lower == upper) {
              if (this.topGreater(event1, event2))
                tcn.addEdge(event1, event2, new Constraint(lower, upper))
              else tcn.addEdge(event2, event1,  new Constraint(lower, upper))
            }
            else if ( dist12 >= dist21 ) {
              tcn.addEdge(event1, event2, new Constraint(lower, upper))
            }
            else {
              tcn.addEdge(event2, event1, new Constraint(lower, upper))
            }
          //}

        }
        // this.minNetwork = tcn

        Some(tcn)
      }
  }

  def pathExists(source : String, dest : String, expr : String, time : Double) : Option[Path] = {
    val eval = (n1 : Double, n2: Double) => expr match {
      case "<" => n1 < n2
      case "<=" => n1 <= n2
      case ">" => n1 > n2
      case ">=" => n1 >= n2
    }

    val evalPath = (lower : Double, upper: Double) => {
      if (expr == "<" || expr == "<=") eval(lower, time)
      else upper == this.inf || eval(upper, time)
    }

    val startNode = source + "-b"
    val endNode = dest + "-b"

    val res : Option[Path] = for(min <- this.findMinimalNetwork();
                                 edge <- min.getEdge(startNode, endNode);
                                 constr <- edge.constr;
                                 lower = constr.lower;
                                 upper = constr.upper;
                                 if evalPath(lower, upper)
                                 ) yield { this.getPath(source, dest) }

    res
  }

  def forAllPaths(source : String,  dest : String, expr : String, time : Double) = {
    val eval = (n1 : Double, n2: Double) => expr match {
      case "<" => n1 < n2
      case "<=" => n1 <= n2
      case ">" => n1 > n2
      case ">=" => n1 >= n2
    }

    val evalPath = (lower : Double, upper: Double) => {
      if (expr == "<" || expr == "<=") upper != this.inf && eval(upper, time)
      else eval(lower, time)
    }

    val startNode = source + "-b"
    val endNode = dest + "-b"

    val res : Option[Path] = for(min <- this.findMinimalNetwork();
                                 edge <- min.getEdge(startNode, endNode);
                                 constr <- edge.constr;
                                 lower = constr.lower;
                                 upper = constr.upper;
                                 if evalPath(lower, upper)
                                 ) yield { this.getPath(source, dest) }
    res
  }

}
