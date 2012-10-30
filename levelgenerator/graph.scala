package graph

trait Vertex {
  def neighbours(edges: List[Edge]) = {
    val ns = edges.filter(_ contains this).map(_ otherVertex this).distinct
    assert(ns.distinct.size == ns.size)
    assert( !(ns contains this) )
    ns
  }
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
  def vA: Vertex
  def vB: Vertex
  def vC: Vertex
}

class EuclideanVertex(val point: geometry.Vec2) extends Vertex with Ordered[EuclideanVertex] {
  import geometry._
  def compare(that: EuclideanVertex) = this.point compare that.point
  def distance(that: EuclideanVertex) = if( this.point == that.point ) 0 else LineSegment(this.point, that.point).length
  override def neighbours(edges: List[Edge]) = super.neighbours(edges).asInstanceOf[List[EuclideanVertex]]
}

class EuclideanEdge(val vA: EuclideanVertex, val vB: EuclideanVertex) extends Edge {
  import geometry._
  def lineSegment = LineSegment(vA.point, vB.point)
  def intersects(that: EuclideanEdge) = this.lineSegment intersects that.lineSegment
}

object EuclideanTriangle {
  import geometry._
  def apply(vertices: List[EuclideanVertex]) = {
 	  assert(vertices.distinct.size == vertices.size)
 	  assert(vertices.map(_.point).distinct.size == vertices.size)
    vertices.sorted match {
      case vs@List(a, b, c) =>
        if (Line(a.point, b.point) ccw c.point)
          new EuclideanTriangle(a, b, c)
        else
          new EuclideanTriangle(a, c, b)
      case _ => sys.error("Needed 3 distinct vertices:\n%s" format vertices)
    }
  }
}
case class EuclideanTriangle(vA: EuclideanVertex, vB: EuclideanVertex, vC: EuclideanVertex) extends geometry.TriangleLike with Triangle {
  def a = vA.point
  def b = vB.point
  def c = vC.point
  
  def inside(p: EuclideanVertex) = super.inside(p.point)
  def contains(p: EuclideanVertex) = vertices contains p
  def vertices = List(vA, vB, vC)
  def angleAt(p: EuclideanVertex) = super.angleAt(p.point)
  def neighbours(triangles:List[EuclideanTriangle]) = {
    List((vA,vB),(vB,vC),(vC,vA)).flatMap{ case (v1,v2) =>
      triangles.filterNot(_==this).find(t => (t contains v1) && (t contains v2))
    }
  }
  def edgesWithoutNeighbours(triangles:List[EuclideanTriangle]) = {
    List((vA,vB),(vB,vC),(vC,vA)).filterNot{ case (v1,v2) =>
      triangles.filterNot(_==this).exists(t => (t contains v1) && (t contains v2))
    }
  }

}

trait EuclideanGraph extends Graph {
  import geometry._
  def vertices: List[EuclideanVertex]

  def minimumSpanningTree = {
    // https://de.wikipedia.org/wiki/Algorithmus_von_Kruskal#Algorithmus
    var edges: List[EuclideanEdge] = Nil
    //TODO: use delaunayEdges instead of complete graph
    var L = vertices.combinations(2).collect { case List(a, b) => new EuclideanEdge(a, b) }.toList.sortBy(_.lineSegment.length)
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

    def getNext: Option[List[EuclideanTriangle]] = {
      val triangles = vertices.flatMap(nA => nA.neighbours(edges).combinations(2).filter {
        case List(nB, nC) =>
          assert(nB != nC)
          assert(nA != nB && nA != nC)
          edges.exists(c => (c contains nB) && (c contains nC))
        case _            => false
      }.collect {
        case List(nB, nC) =>
          assert(nB != nC)
          assert(nA != nB && nA != nC)
          EuclideanTriangle(List(nA, nB, nC))
      }).distinct.filterNot(t => vertices.exists(t inside _))

      val connectedTriangles = triangles.combinations(2).filter {
        case List(tA, tB) => tA connectedTo tB
        case _            => false
      }

      val notMeetingDelaunayCondition = connectedTriangles.find {
        case List(tA, tB) =>
          val sharedVertices = tA.vertices intersect tB.vertices
          val angleA = tA.angleAt((tA.vertices diff sharedVertices).head)
          val angleB = tB.angleAt((tB.vertices diff sharedVertices).head)
          angleA + angleB > math.Pi
        case _ => false
      }

      notMeetingDelaunayCondition
    }
    // iterate over all connected triangles
    var next: Option[List[EuclideanTriangle]] = null
    while ({ next = getNext; next.nonEmpty }) {
      val List(tA, tB) = next.get
      val sharedVertices = tA.vertices intersect tB.vertices
      val notSharedVertices = (tA.vertices union tB.vertices).distinct diff sharedVertices
      edges = edges.filterNot(e => (sharedVertices contains e.vA) && (sharedVertices contains e.vB))
      edges ::= new EuclideanEdge(notSharedVertices(0), notSharedVertices(1))
      next = getNext
    }

    edges
  }
  
  def voronoiDiagram(bounds:ConvexPolygonLike) = {
  
    val delaunay = delaunayEdges
    val triangles = vertices.flatMap(nA => nA.neighbours(delaunay).combinations(2).filter {
        case List(nB, nC) => delaunay.exists(c => (c contains nB) && (c contains nC))
        case _            => false
      }.collect {
        case List(nB, nC) => EuclideanTriangle(List(nA, nB, nC))
      }).distinct.filterNot(t => vertices.exists(t inside _))

    val connectedTriangles = (triangles.combinations(2).filter {
      case List(tA, tB) => tA connectedTo tB
      case _            => false
    }).toList
    
    var cells = new collection.mutable.HashMap[EuclideanVertex,List[Vec2]]()
    
    // normal cell edges, clipped to bounds
    for( List(tA,tB) <- connectedTriangles ) {
      val sharedVertices = tA.vertices intersect tB.vertices
      for( vertex <- sharedVertices ) {
        LineSegment(tA.circumcenter, tB.circumcenter).clip(bounds) match {
          case Some(LineSegment(a,b)) =>
            cells(vertex) = a :: b :: (if(!cells.isDefinedAt(vertex)) Nil else cells(vertex))
          case None =>
        }
      }
    }

    // infinite cell edges, clipped to bounds
    for( t <- triangles; (a,b) <- t.edgesWithoutNeighbours(triangles) ) {
        val edge = LineSegment(a.point,b.point)
        val c = t.circumcenter
                
        val ray = Ray(c, c + edge.normal)
        val intersection = ray firstIntersectionPoint bounds
        intersection match {
          case Some(p) =>
            LineSegment(c,p).clip(bounds) match {
              case Some(LineSegment(c,p)) =>
                cells(a) = c :: p :: (if(!cells.isDefinedAt(a)) Nil else cells(a))
                cells(b) = c :: p :: (if(!cells.isDefinedAt(b)) Nil else cells(b))
              case _ =>
            }
          case _ =>
        }
    }


    val polygons = cells.map{ case (v,p) => v -> ConvexPolygon.fromUnsorted(p) }



    
    val lines = connectedTriangles.flatMap{ case List(tA,tB) =>
      LineSegment(tA.circumcenter,tB.circumcenter).clip(bounds)
    }
    
    val infinityLines:List[LineSegment] = triangles.flatMap(t => t.edgesWithoutNeighbours(triangles).flatMap{
      case (a,b) =>
        val edge = LineSegment(a.point,b.point)
        val c = t.circumcenter
        val ray = Ray(c, c+edge.normal)
        val intersection = ray firstIntersectionPoint bounds
        intersection match {
          case Some(p) => Some(LineSegment(c, p))
          case None => None
        }
        //Line(c, (line rayIntersectionPoint boundingRect).get )
        //Line(c, edge.midPoint + edge.normal.normalized*(maxWidth + maxHeight))
    })
    
    
    (polygons, lines ++ infinityLines)
  }
}
