package venture

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Color
import java.awt.Font
import javax.imageio.ImageIO

import java.io.File

object LevelGenerator extends App {

  for( i <- 0 until 1 ) {
    val graph = new Dungeon(i)
    println("Drawing Graph %3d..." format i)
    (new File("out")).mkdirs()
    graph.drawToImage("out/test%03d.png" format i)
  }
  
  
  println("done")
}

object Config {
  val width = 500
  val height = 500
  val branchCount = 15
  val maxDegree = 4
  val minDistance = 90
  val edgeProbabilities = Array(0.9,0.6,0.3)
  val minLineBranchDistance = 60 //TODO: statt Pixelabstand, den Winkel vom erzeugten Dreieck limitieren
  
  val branchDiameter = 40
}

case class Vec2(x:Double, y:Double) {
  import math._
  def +(that:Vec2) = Vec2(this.x + that.x, this.y + that.y)
  def -(that:Vec2) = Vec2(this.x - that.x, this.y - that.y)
  def *(that:Double) = Vec2(this.x * that, this.y * that)
  def /(that:Double) = Vec2(this.x / that, this.y / that)
  def dot(that:Vec2) = this.x * that.x + this.y * that.y
  def length = sqrt(x*x+y*y)
  def normalized = this / length
}

case class Line(start:Vec2, end:Vec2) {
  def dir = end-start
  def normal = Vec2(dir.y,-dir.x)
  def apply(t:Double) = start + dir*t
  def length = dir.length
  def intersection(that:Line) = {
    // t = (A-P).n / (A-B).n
    val d = this.dir dot that.normal
    if( d == 0 ) None
    else Some( ((that.end - this.start) dot that.normal)/d )
  }
  
  def ccw(p:Vec2) = (start.x - p.x) * (end.y - p.y) - (start.y - p.y) * (end.x - p.x) > 0

  def intersects(that:Line) = {
    (this.intersection(that), that.intersection(this)) match {
      case (Some(s),Some(t)) =>
        0 < t && t < 1 && 0 < s && s < 1
      case _ => false
    }
  }
  
  def distance(p:Vec2) = {
    val v = dir.normalized
    val t = (p-start) dot v
    if( 0 < t && t < length )
      (apply(t/length) - p).length
    else
      Double.MaxValue
  }
}

class Dungeon(seed:Any) {
  import Config._
  import math._
  
  private val rng = new util.Random(seed.hashCode)
  
  case class Branch(x:Int, y:Int, seed:Int) {
    var id:Int = 0
    def neighbours:List[Branch] = neighbours(connections)
    def neighbours(edges:List[BranchConnection] = connections) = edges.filter(_ contains this).map(_ otherBranch this).distinct
    def degree = neighbours.size
    def point = Vec2(x,y)
    override def toString = "Branch(%d)" format id
  }

  case class BranchConnection(nA:Branch, nB:Branch) {
    def contains(n:Branch) = (n == nA || n == nB)
    def otherBranch(n:Branch) = { require(contains(n)); if( n == nA) nB else nA }
    def line = Line(Vec2(nA.x, nA.y), Vec2(nB.x, nB.y))
    def intersects(that:BranchConnection) = this.line intersects that.line
  }
  
  var branches:List[Branch] = Nil
  var connections:List[BranchConnection] = Nil
//  var dependencies:List[BranchConnection] = Nil
  
  private def rInt = rng.nextInt & Int.MaxValue
  private def rDouble = rng.nextDouble
  
  def randomBranch = branches(rInt % branches.size)
  def branchDistance(n1:Branch, n2:Branch) = sqrt(pow(n1.x-n2.x,2)+pow(n1.y-n2.y,2))

  def isConnected = {
    var visited:List[Branch] = Nil
    var next = branches.take(1)
    while( next.nonEmpty ) {
      val current = next.head
      next = next.tail
      if( !(visited contains current) ) {
        visited ::= current
        next :::= current.neighbours
      }
    }
    visited.size == branches.size
  }
  
  def hasCycle = {
    var visited:List[Branch] = Nil
    var finished:List[Branch] = Nil
    var foundCycle = false
    for( branch <- branches ) {
      dfs(branch, branch)
    }
    
    def dfs(v:Branch, source:Branch) {
      if(finished contains v)
        return
      if((visited contains v)) {
        foundCycle = true
        return
      }
      visited ::= v
      for( w <- v.neighbours if(w != source) )
        dfs(w, v)
      finished ::= v
    }
    
    foundCycle
  }


  def delaunayEdges = {
    var edges:List[BranchConnection] = Nil
    // initial triangle mesh
    for( branch <- branches ) {
      val closest = branches.sortBy(other => branchDistance(branch, other)).tail
      val candidates = closest.map( other => BranchConnection(branch, other) )
      edges :::= candidates.filterNot( c => edges.exists(_ == c) || edges.exists(_.intersects(c) ) )
    }
    
    object Triangle {
      def apply(branches:List[Branch]) = {
        branches.sortBy(_.id) match {
          case List(a,b,c) =>
            if(Line(a.point, b.point) ccw c.point)
              new Triangle(a,b,c)
            else
              new Triangle(a,c,b)
        }
      }
    }
    case class Triangle(nA:Branch, nB:Branch, nC:Branch) {
      require(nA != nB && nB != nC && nC != nA, "Two points are the same: %s %s %s" format(nA, nB, nC))
      require(nA.id < nB.id && nA.id < nC.id, "Lowest Id needs to be first: %d %d %d" format(nA.id, nB.id, nC.id))
      require(Line(nA.point, nB.point).ccw(nC.point) &&
              Line(nB.point, nC.point).ccw(nA.point) &&
              Line(nC.point, nA.point).ccw(nB.point), "Points not in ccw order: %s %s %s" format(nA, nB, nC) )
      def vertices = List(nA,nB,nC)
      def connectedTo(that:Triangle) = (this.vertices intersect that.vertices).size == 2
      def angleAt(n:Branch) = {
        if( n == nA ) angle(nC, nA, nB)
        else if( n == nB ) angle(nA, nB, nC)
        else angle(nB, nC, nA)
      }
      def angle(n1:Branch,n2:Branch,n3:Branch) = {
        val a = n1.point - n2.point
        val b = n3.point - n2.point
        acos( (a dot b) / (a.length * b.length) )
      }
      def contains(p:Branch) = {
        Line(nA.point, nB.point).ccw(p.point) &&
        Line(nB.point, nC.point).ccw(p.point) &&
        Line(nC.point, nA.point).ccw(p.point)
      }
    }
    
    // iterate over all connected triangles
    def getNext:List[List[Triangle]] = {
      val triangles = branches.flatMap(nA => nA.neighbours(edges).combinations(2).filter{
        case List(nB,nC) => edges.exists(c => (c contains nB) && (c contains nC)) }.map{
        case List(nB,nC) => Triangle(List(nA,nB,nC))
      }).distinct.filterNot(t => branches.exists(t contains _))
      
      println(triangles.mkString("\n"))
      
      val connectedTriangles = triangles.combinations(2).filter{
        case List(tA,tB) => tA connectedTo tB
      }
      val notMeetingDelaunayCondition = connectedTriangles.filter{
        case List(tA,tB) =>
          val sharedVertices = tA.vertices intersect tB.vertices
          val angleA = tA.angleAt((tA.vertices diff sharedVertices).head)
          val angleB = tB.angleAt((tB.vertices diff sharedVertices).head)
          angleA + angleB > Pi
      }
      
      notMeetingDelaunayCondition.toList
    }
    var next:List[List[Triangle]] = null
    while( {next = getNext;next.nonEmpty} ) {
      println(next.size)
      val List(tA,tB) = next.head
      val sharedVertices = tA.vertices intersect tB.vertices
      val notSharedVertices = (tA.vertices union tB.vertices).distinct diff sharedVertices
      edges = edges.remove{case BranchConnection(nA, nB) => (sharedVertices contains nA) && (sharedVertices contains nB) }
      edges ::= BranchConnection(notSharedVertices(0), notSharedVertices(1))
      next = getNext
    }
    
    edges
  }
  

  
  // Branches 
  for( i <- 0 until branchCount ) {
    var newBranch:Branch = null
    do {
      newBranch = Branch(rInt % width, rInt % height, seed=rInt)
      newBranch.id = i
    } while(
      branches.exists(d =>
        branchDistance(d, newBranch) < minDistance
        ) ||
      newBranch.x-branchDiameter/2 < 0 ||
      newBranch.x+branchDiameter/2 > width ||
      newBranch.y-branchDiameter/2 < 0 ||
      newBranch.y+branchDiameter/2 > height
    )
    branches ::= newBranch
  }
  
  val startBranch = randomBranch // choose branch with highest degree?

  val delaunay = delaunayEdges






  // Connections (minimum spanning tree on complete graph)
  var L = branches.combinations(2).collect{ case List(a,b) => BranchConnection(a,b) }.toList.sortBy{ case BranchConnection(a,b) => branchDistance(a,b) }
  while(L.nonEmpty) {
    val e = L.head
    L = L.tail
    connections ::= e
    if(hasCycle)
      connections = connections.tail
  }
  
  // More Connections (usually creating cycles and have restrictions)
  for( branch <- branches ) {
    val d1 = branch
    val close = branches.sortBy(d => branchDistance(d1,d)).tail zip edgeProbabilities
    val d2s = close.filter(rDouble <= _._2).map(_._1)
    for( (connection,d2) <- d2s.map(d2 => (BranchConnection(d1,d2),d2)) ) {
      if( !connections.exists(_.intersects(connection) ) &&
        !branches.filterNot(d => d == connection.nA || d == connection.nB).exists{d => connection.line.distance(d.point) < Config.minLineBranchDistance} &&
        d1.degree <= maxDegree && d2.degree <= maxDegree
       )
        connections ::= connection
    }
  }
  
  // Game path (choose the closest one possible)
  /*var gamePath:List[Branch] = List(startBranch)
  while( gamePath.size < branches.size ) {
    val candidates = gamePath.flatMap(_.neighbours).distinct.diff(gamePath)
    gamePath ::= candidates.minBy(n => branchDistance(n,gamePath.head))
  }
  for( (branch,i) <- gamePath.reverse zipWithIndex )
    branch.id = i*/
  
  
  def drawToImage(filename:String) {
    val backgroundColor = new Color(0xEEEEEE)
    val branchColor = new Color(0xCCCCCC)
    val startColor = new Color(0x00A020)
    val connectionColor = new Color(0x999999)
    val contourColor = new Color(0x666666)
    val textColor = new Color(0x333333)
    val textFont = new Font("Sans", Font.BOLD, 20)

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g:Graphics2D = image.createGraphics
    import g._
    setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON)
    setStroke(new java.awt.BasicStroke(3))
    setFont(textFont)
    val frc = getFontRenderContext
    def stringBounds(s:String) = {
      val bounds = textFont.getStringBounds(s, frc)
      val metrics = textFont.getLineMetrics(s, frc)
      Vec2(bounds.getWidth, metrics.getHeight)
    }

    def fillCircle(x:Int, y:Int, diameter:Int) = fillOval(x-diameter/2, y-diameter/2, diameter, diameter)
    def drawCircle(x:Int, y:Int, diameter:Int) = drawOval(x-diameter/2, y-diameter/2, diameter, diameter)
    def drawBranch(branch:Branch, color:Color) = {
      setColor(color)
      fillCircle(branch.x, branch.y, branchDiameter)
      setColor(contourColor)
      drawCircle(branch.x, branch.y, branchDiameter)
      
      setColor(textColor)
      val string = branch.id.toString
      val bounds = stringBounds(string)
      drawString(string, (branch.x - bounds.x/2).toInt, (branch.y + bounds.y / 2).toInt)
    }
    
    def drawBranchConnection(edge:BranchConnection, color:Color) {
      setColor(color)
      drawLine(edge.nA.x, edge.nA.y, edge.nB.x, edge.nB.y)
    }
    

    setBackground(backgroundColor)
    clearRect(0,0,width,height)
    
    for( connection <- connections ) {
/*      if( connections.exists(_.intersects(connection)) || 
          branches.filterNot(d => d == connection.nA || d == connection.nB).exists{d => connection.line.distance(d.point) < Config.minLineBranchDistance}
       )
        drawBranchConnection(connection, new Color(0xFF0000))
      else*/
        drawBranchConnection(connection, connectionColor)
    }

    for( connection <- delaunay )
        drawBranchConnection(connection, new Color(0xAA6622))


    for( branch <- branches )
      drawBranch(branch,branchColor)
    
    drawBranch(startBranch, startColor)
    
    
    val outputfile = new File(filename)
    ImageIO.write(image, "png", outputfile)
  }
}







