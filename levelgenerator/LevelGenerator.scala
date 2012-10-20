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

class Dungeon(seed:Any) {
  import Config._
  import graph._
  import geometry._
  
  private val rng = new util.Random(seed.hashCode)
  
  case class Branch(point:Vec2, seed:Int) extends PointVertex {
    var id:Int = 0
    override def toString = "Branch(%d)" format id
  }

  case class BranchConnection(nA:Branch, nB:Branch) extends PointEdge
  
  var branches:List[Branch] = Nil
  var connections:List[BranchConnection] = Nil
  
  private def rInt = rng.nextInt & Int.MaxValue
  private def rDouble = rng.nextDouble
  
  def randomBranch = branches(rInt % branches.size)



  

  
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







