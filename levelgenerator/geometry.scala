package geometry

import math._

case class Vec2(x: Double, y: Double) extends Ordered[Vec2] {
	def compare(that: Vec2) = {
		val d = this.x compare that.x
		if (d != 0)
			d
		else this.y compare that.y
	}

	def +(that: Vec2) = Vec2(this.x + that.x, this.y + that.y)
	def -(that: Vec2) = Vec2(this.x - that.x, this.y - that.y)
	def *(that: Double) = Vec2(this.x * that, this.y * that)
	def /(that: Double) = Vec2(this.x / that, this.y / that)

	def dot(that: Vec2) = this.x * that.x + this.y * that.y
	def length = sqrt(x * x + y * y)
	def normalized = this / length
}

trait LineLike {
  def a:Vec2
  def b:Vec2
  require(a != b)
  
	def dir = b - a
	def apply(t: Double) = a + dir * t
	def normal = Vec2(dir.y, -dir.x)

	def intersection(that: LineLike):Option[Double] = {
		// t = (A-P).n / (A-B).n
		val d = this.dir dot that.normal
		if (d == 0) None
		else Some(((that.b - this.a) dot that.normal) / d)
	}
	
	def intersections(polygon:ConvexPolygonLike):List[Double] = {
	  polygon.edges.filter{ e =>
		  val t = (e intersection this)
		  t.isDefined && t.get >= 0 && t.get <= 1
		}.flatMap( this intersection _ )
	}
	
	def intersectionPoints(polygon:ConvexPolygonLike) = intersections(polygon).map(apply)
	
	def ccw(p: Vec2) = (a.x - p.x) * (b.y - p.y) - (a.y - p.y) * (b.x - p.x) > 0
}

case class Line(a:Vec2, b:Vec2) extends LineLike

case class Ray(a:Vec2, b:Vec2) extends LineLike {
	override def intersections(polygon: ConvexPolygonLike) = 	intersections(polygon).filter(_ >= 0)
  override def intersectionPoints(polygon: ConvexPolygonLike) = intersections(polygon).map(apply)
  def firstIntersectionPoint(polygon: ConvexPolygonLike):Option[Vec2] = {
    val is = intersections(polygon)
    if( is.nonEmpty )
      Some(apply(is.min))
    else
      None
  }
}

case class LineSegment(a:Vec2, b:Vec2) extends LineLike {
	def midPoint = (a + b) * 0.5
	def length = dir.length

	override def intersections(polygon: ConvexPolygonLike) = intersections(polygon).filter(d => d >= 0 && d <= 1)
  override def intersectionPoints(polygon: ConvexPolygonLike) = intersections(polygon).map(apply)

	def intersects(that: LineSegment) = {
		(this.intersection(that), that.intersection(this)) match {
			case (Some(s), Some(t)) =>
				0 < t && t < 1 && 0 < s && s < 1
			case _ => false
		}
	}

	def distance(p: Vec2) = {
		val v = dir.normalized
		val t = (p - a) dot v
		if (0 < t && t < length)
			(apply(t / length) - p).length
		else
			Double.MaxValue
	}

  def clip(polygon:ConvexPolygonLike):LineSegment = {
    val is = intersections(polygon).sorted
    assert( is.size == 0 || is.size == 2 )
    is match {
      case List(t1,t2) => LineSegment(apply(t1), apply(t2))
      case Nil => this
    }
  }
}

trait ConvexPolygonLike extends collection.SeqProxy[Vec2] {
  def points:List[Vec2]
  
  def self = points
  private def rotatedSlidingWindow(n:Int) = (points ::: points.take(n-1)).sliding(n)
  def edges = rotatedSlidingWindow(2).map{ case List(a,b) => LineSegment(a,b) }.toList
  def triples = rotatedSlidingWindow(3)
  require( points.size == points.distinct.size, "No degenerated Polygons allowed:\n%s" format(points) )
  require( points.distinct.size >= 3, "Not enough Points: %d\n%s" format (points.size, points) )
  require( triples.forall{ case List(a,b,c) => Line(a,b).ccw(c) }, "Points not in ccw order:\n%s\n%s" format (points, points.map(p => {val Vec2(x,y) = (points.head-p); math.atan2(y,x)})))
}

case class ConvexPolygon(points:List[Vec2]) extends ConvexPolygonLike

object ConvexPolygon {
  def fromUnsorted(points:List[Vec2]) = {
    val centre = points.reduce(_ + _) / points.size
    new ConvexPolygon(points.distinct.sortBy{v =>
      val Vec2(x,y) = (centre - v)
      math.atan2(y,x)
    })
  }
}


object Triangle {
	def fromUnsorted(points: List[Vec2]) = {
		points.sorted match {
			case List(a, b, c) =>
				if (Line(a, b) ccw c)
					new Triangle(a, b, c)
				else
					new Triangle(a, c, b)
			case _ => sys.error("Needed 3 Points, given: %s" format points)
		}
	}
}

trait TriangleLike extends ConvexPolygonLike {
  def a: Vec2
  def b: Vec2
  def c: Vec2

	require(a != b && b != c && c != a,
		"Two points are the same: %s %s %s" format (a, b, c))
	require(a <= b && a <= c,
		"Point with lowest coordinates needs to be first: %d %d %d" format (a, b, c))
	require(Line(a, b).ccw(c) &&
		Line(b, c).ccw(a) &&
		Line(c, a).ccw(b),
		"Points not in ccw order: %s %s %s" format (a, b, c))
	//def points = List(a, b, c)
	def connectedTo(that: TriangleLike) = (this.points intersect that.points).size == 2
	def angleAt(p: Vec2) = {
		def angle(p1: Vec2, p2: Vec2, p3: Vec2) = {
			val a = p1 - p2
			val b = p3 - p2
			acos((a dot b) / (a.length * b.length))
		}
		if (p == a) angle(c, a, b)
		else if (p == b) angle(a, b, c)
		else if (p == c) angle(b, c, a)
		else sys.error("Point %s not in %s" format (p, this))
	}

	def inside(p: Vec2) = {
		//TODO: def ccw(p1:Vec2, p2:Vec2, p3:Vec2)
		Line(a, b).ccw(p) &&
			Line(b, c).ccw(p) &&
			Line(c, a).ccw(p)
	}

	def circumcenter = {
		val d = 2 * (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y))
		val x = ((a.x * a.x + a.y * a.y) * (b.y - c.y) + (b.x * b.x + b.y * b.y) * (c.y - a.y) + (c.x * c.x + c.y * c.y) * (a.y - b.y)) / d
		val y = ((a.x * a.x + a.y * a.y) * (c.x - b.x) + (b.x * b.x + b.y * b.y) * (a.x - c.x) + (c.x * c.x + c.y * c.y) * (b.x - a.x)) / d
		Vec2(x, y)
	}
}

case class Triangle(a:Vec2, b:Vec2, c:Vec2)

case class Rectangle(a:Vec2, b:Vec2) extends ConvexPolygonLike {
  def c = Vec2(b.x,a.y)
  def d = Vec2(a.x,b.y)
  def points = List(a,c,b,d)
  //a--d
  //|  |
  //c--b
  def width = (a.x-b.x).abs
  def height = (a.y-b.y).abs
  //require( width > 0 && height > 0, "No degenerated rectangles allowed." )
  //def edges = List(Line(a,c),Line(c,b), Line(b,d), Line(d,a))
}

