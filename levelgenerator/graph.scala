package graph

trait Vertex {
  def neighbours(edges: List[Edge]) = edges.filter(_ contains this).map(_ otherVertex this).distinct
  def degree(edges: List[Edge]) = neighbours(edges).size
}

trait Edge {
  def vA: Vertex
  def vB: Vertex
  def contains(v: Vertex) = (v == vA || v == vB)
  def otherVertex(v: Vertex) = { require(contains(v)); if (v == vA) vB else vA }
}

trait Graph {
  def vertices: List[Vertex]

  def isConnected(edges: List[Edge]) = {
    var visited: List[Vertex] = Nil
    var next = vertices.take(1)
    while (next.nonEmpty) {
      val current = next.head
      next = next.tail
      if (!(visited contains current)) {
        visited ::= current
        next :::= current.neighbours(edges)
      }
    }
    visited.size == edges.size
  }

  def hasCycle(edges: List[Edge]) = {
    var visited: List[Vertex] = Nil
    var finished: List[Vertex] = Nil
    var foundCycle = false
    for (vertex <- vertices) {
      dfs(vertex, vertex)
    }

    def dfs(v: Vertex, source: Vertex) {
      if (finished contains v)
        return
      if ((visited contains v)) {
        foundCycle = true
        return
      }
      visited ::= v
      for (w <- v.neighbours(edges) if (w != source))
        dfs(w, v)
      finished ::= v
    }

    foundCycle
  }

}

trait Triangle {
  def a: Vertex
  def b: Vertex
  def c: Vertex
}

class EuclideanVertex(val point: geometry.Vec2) extends Vertex with Ordered[EuclideanVertex] {
  import geometry._
  def compare(that: EuclideanVertex) = this.point compare that.point
  def distance(that: EuclideanVertex) = Line(this.point, that.point).length
  override def neighbours(edges: List[Edge]) = super.neighbours(edges).asInstanceOf[List[EuclideanVertex]]
}

class EuclideanEdge(val vA: EuclideanVertex, val vB: EuclideanVertex) extends Edge {
  import geometry._
  def line = Line(vA.point, vB.point)
  def intersects(that: EuclideanEdge) = this.line segmentIntersects that.line
}

object EuclideanTriangle {
  import geometry._
  def apply(vertices: List[EuclideanVertex]) = {
    vertices.sorted match {
      case List(a, b, c) =>
        if (Line(a.point, b.point) ccw c.point)
          new EuclideanTriangle(a, b, c)
        else
          new EuclideanTriangle(a, c, b)
      case _ => sys.error("Needed 3 Points, given: %s" format vertices)
    }
  }
}
case class EuclideanTriangle(a: EuclideanVertex, b: EuclideanVertex, c: EuclideanVertex) extends geometry.Triangle(a.point, b.point, c.point) with Triangle {
  def contains(p: EuclideanVertex) = super.contains(p.point)
  def vertices = List(a, b, c)
  def angleAt(p: EuclideanVertex) = super.angleAt(p.point)
}

trait EuclideanGraph extends Graph {
  import geometry._
  def vertices: List[EuclideanVertex]

  def minimumSpanningTree = {
    // https://de.wikipedia.org/wiki/Algorithmus_von_Kruskal#Algorithmus
    var edges: List[EuclideanEdge] = Nil
    //TODO: use delaunayEdges instead of complete graph
    var L = vertices.combinations(2).collect { case List(a, b) => new EuclideanEdge(a, b) }.toList.sortBy(_.line.length)
    while (L.nonEmpty) {
      val e = L.head
      L = L.tail
      edges ::= e
      if (hasCycle(edges))
        edges = edges.tail
    }
    edges
  }

  def delaunayEdges = {
    var edges: List[EuclideanEdge] = Nil
    // initial triangle mesh
    for (vertex <- vertices) {
      val closest = vertices.sortBy(_ distance vertex).tail
      val candidates = closest.map(new EuclideanEdge(vertex, _))
      edges :::= candidates.filterNot(e => edges.exists(_ == e) || edges.exists(_ intersects e))
    }

    def getNext: List[List[EuclideanTriangle]] = {
      val triangles = vertices.flatMap(nA => nA.neighbours(edges).combinations(2).filter {
        case List(nB, nC) => edges.exists(c => (c contains nB) && (c contains nC))
        case _            => false
      }.collect {
        case List(nB, nC) => EuclideanTriangle(List(nA, nB, nC))
      }).distinct.filterNot(t => vertices.exists(t contains _))

      val connectedTriangles = triangles.combinations(2).filter {
        case List(tA, tB) => tA connectedTo tB
        case _            => false
      }

      val notMeetingDelaunayCondition = connectedTriangles.filter {
        case List(tA, tB) =>
          val sharedVertices = tA.vertices intersect tB.vertices
          val angleA = tA.angleAt((tA.vertices diff sharedVertices).head)
          val angleB = tB.angleAt((tB.vertices diff sharedVertices).head)
          angleA + angleB > math.Pi
        case _ => false
      }

      notMeetingDelaunayCondition.toList
    }
    // iterate over all connected triangles
    var next: List[List[EuclideanTriangle]] = null
    while ({ next = getNext; next.nonEmpty }) {
      val List(tA, tB) = next.head
      val sharedVertices = tA.vertices intersect tB.vertices
      val notSharedVertices = (tA.vertices union tB.vertices).distinct diff sharedVertices
      edges = edges.filterNot(e => (sharedVertices contains e.vA) && (sharedVertices contains e.vB))
      edges ::= new EuclideanEdge(notSharedVertices(0), notSharedVertices(1))
      next = getNext
    }

    edges
  }
}
