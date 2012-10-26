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

case class Line(start: Vec2, end: Vec2) {
	def midPoint = (start + end) * 0.5
	def dir = end - start
	def apply(t: Double) = start + dir * t
	def length = dir.length
	def normal = Vec2(dir.y, -dir.x)

	def intersection(that: Line) = {
		// t = (A-P).n / (A-B).n
		val d = this.dir dot that.normal
		if (d == 0) None
		else Some(((that.end - this.start) dot that.normal) / d)
	}

	def ccw(p: Vec2) = (start.x - p.x) * (end.y - p.y) - (start.y - p.y) * (end.x - p.x) > 0

	def segmentIntersects(that: Line) = {
		(this.intersection(that), that.intersection(this)) match {
			case (Some(s), Some(t)) =>
				0 < t && t < 1 && 0 < s && s < 1
			case _ => false
		}
	}

	def segentDistance(p: Vec2) = {
		val v = dir.normalized
		val t = (p - start) dot v
		if (0 < t && t < length)
			(apply(t / length) - p).length
		else
			Double.MaxValue
	}
}

object Triangle {
	def apply(points: List[Vec2]) = {
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

class Triangle(a: Vec2, b: Vec2, c: Vec2) {
	require(a != b && b != c && c != a,
		"Two points are the same: %s %s %s" format (a, b, c))
	require(a <= b && a <= c,
		"Point with lowest coordinates needs to be first: %d %d %d" format (a, b, c))
	require(Line(a, b).ccw(c) &&
		Line(b, c).ccw(a) &&
		Line(c, a).ccw(b),
		"Points not in ccw order: %s %s %s" format (a, b, c))
	def points = List(a, b, c)
	def connectedTo(that: Triangle) = (this.points intersect that.points).size == 2
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

