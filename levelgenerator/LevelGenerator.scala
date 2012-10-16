package venture

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Color
import javax.imageio.ImageIO

import java.io.File

object LevelGenerator extends App {

  for( i <- 0 until 300 ) {
    val graph = new OverviewGraph
    println("Drawing Graph %3d..." format i)
    graph.drawToImage("out/test%03d.png" format i)
  }
  
  
  println("done")
}

object Config {
  val width = 500
  val height = 500
  val dungeonCount = 15
  //TODO: val maxDegree = 4
  val minDistance = 90
  val edgeProbabilities = Array(0.8,0.5,0.2)
  val minLineNodeDistance = 60
  
  val nodeDiameter = 40
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

class OverviewGraph {
  import Config._
  import math._
  case class Node(x:Int, y:Int, var difficulty:Int = 0) {
    def neighbours:Set[Node] = ways.map(_.set).filter(_ contains this).map(s => (s - this).head)
    def degree = neighbours.size
    def point = Vec2(x,y)
  }
  case class Edge(nA:Node, nB:Node) {
    def set = Set(nA, nB)
    def line = Line(Vec2(nA.x, nA.y), Vec2(nB.x, nB.y))
    def intersects(that:Edge) = this.line intersects that.line
  }
  var dungeons:List[Node] = Nil
  var ways:Set[Edge] = Set.empty
  var dependencies:List[Edge] = Nil
  
  def r = util.Random.nextInt.abs
  def randomDungeon = dungeons(r % dungeons.size)
  def nodeDistance(n1:Node, n2:Node) = sqrt(pow(n1.x-n2.x,2)+pow(n1.y-n2.y,2))
  
  // Knoten 
  for( i <- 0 until dungeonCount ) {
    var newNode:Node = null
    do {
      newNode = Node(r % width, r % height)
    } while(
      dungeons.exists(d => nodeDistance(d, newNode) < minDistance) ||
      newNode.x-nodeDiameter/2 < 0 ||
      newNode.x+nodeDiameter/2 > width ||
      newNode.y-nodeDiameter/2 < 0 ||
      newNode.y+nodeDiameter/2 > height
    )
    dungeons ::= newNode
  }
  
  def isConnected = {
    var visited = Set.empty[Node]
    var next = Set(dungeons.head)
    while( next.nonEmpty ) {
      val current = next.head
      next -= current
      if( !(visited contains current) ) {
        visited += current
        next ++= current.neighbours
      }
    }
    visited.size == dungeons.size
  }
  
  def hasCycle = {
    var visited = Set.empty[Node]
    var finished = Set.empty[Node]
    var foundCycle = false
    for( node <- dungeons ) {
      dfs(node, node)
    }
    
    def dfs(v:Node, source:Node) {
      if(finished contains v)
        return
      if((visited contains v)) {
        foundCycle = true
        return
      }
      visited += v
      for( w <- v.neighbours if(w != source) )
        dfs(w, v)
      finished += v
    }
    
    foundCycle
  }
  
  val startDungeon = randomDungeon
  
  // Kanten (minimaler Spannbaum auf vollständigem Graph)
  var L = dungeons.combinations(2).collect{ case List(a,b) => Edge(a,b) }.toList.sortBy{ case Edge(a,b) => nodeDistance(a,b) }
  while(L.nonEmpty) {
    val e = L.head
    L = L.tail
    ways += e
    if(hasCycle)
      ways -= e
  }
  
  // Weitere Kanten
  for( node <- dungeons ) {
    val d1 = node
    val close = dungeons.sortBy(d => nodeDistance(d1,d)).tail zip edgeProbabilities
    val d2s = close.filter(util.Random.nextDouble <= _._2).map(_._1)
    for( way <- d2s.map(d2 => Edge(d1,d2)) ) {
    	if( !ways.exists(_.intersects(way) ) &&
	    	!dungeons.filterNot(d => d == way.nA || d == way.nB).exists{d => way.line.distance(d.point) < Config.minLineNodeDistance}
    	 )
	    	ways += way
    }
  }
/*    val d1 = connected(r % connected.size)
    //val d2 = dungeons.sortBy(d => nodeDistance(d1,d)).drop(1).zip(edgeProbabilities)
    val close = unconnected.sortBy(d => nodeDistance(d1,d)) zip edgeProbabilities.scan(0)(_+_).tail
    val random = r % edgeProbabilities.sum
    val d2 = close.find(random < _._2).get._1
    connected ::= d2
    ways += Edge(d1,d2)*/
  
  
  def drawToImage(filename:String) {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g:Graphics2D = image.createGraphics
    import g._
    setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON)
    setStroke(new java.awt.BasicStroke(3))
    
    val backgroundColor = new Color(0xEEEEEE)
    val dungeonColor = new Color(0xCCCCCC)
    val startColor = new Color(0x00A020)
    val wayColor = new Color(0x999999)
    val contourColor = new Color(0x666666)
    

    setBackground(backgroundColor)
    clearRect(0,0,width,height)
    
    for( way <- ways ) {
      if( ways.exists(_.intersects(way)) || 
          dungeons.filterNot(d => d == way.nA || d == way.nB).exists{d => way.line.distance(d.point) < Config.minLineNodeDistance}
       )
        setColor(new Color(0xFF0000))
      else
        setColor(wayColor)
      drawLine(way.nA.x, way.nA.y, way.nB.x, way.nB.y)
    }

    for( dungeon <- dungeons ) {
      setColor(dungeonColor)
      fillOval(dungeon.x-nodeDiameter/2, dungeon.y-nodeDiameter/2, nodeDiameter, nodeDiameter)
      setColor(contourColor)
      drawOval(dungeon.x-nodeDiameter/2, dungeon.y-nodeDiameter/2, nodeDiameter, nodeDiameter)
    }
    
    setColor(startColor)
    fillOval(startDungeon.x-nodeDiameter/2, startDungeon.y-nodeDiameter/2, nodeDiameter, nodeDiameter)
    setColor(contourColor)
    drawOval(startDungeon.x-nodeDiameter/2, startDungeon.y-nodeDiameter/2, nodeDiameter, nodeDiameter)
    
    
    val outputfile = new File(filename)
    ImageIO.write(image, "png", outputfile)
  }
}







