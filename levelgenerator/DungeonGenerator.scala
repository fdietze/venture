package venture.dungeonGenerator

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Color
import java.awt.Font
import javax.imageio.ImageIO

import graph._
import geometry._

object Config {
  val width = 500
  val height = 500
  val branchCount = 15
  val maxDegree = 4
  val minDistance = 90
  val edgeProbabilities = Array(0.9, 0.6, 0.3)
  val minLineBranchDistance = 60 //TODO: statt Pixelabstand, den Winkel vom erzeugten Dreieck limitieren

  val branchDiameter = 40
}

case class Branch(_point: Vec2, seed: Int) extends EuclideanVertex(_point) {
  def x = point.x.toInt
  def y = point.y.toInt
  var id: Int = 0
  override def toString = "Branch(%d)" format id
}

case class BranchConnection(nA: Branch, nB: Branch) extends EuclideanEdge(nA, nB)

class Dungeon(seed: Any) extends EuclideanGraph {
  import Config._

  private val rng = new util.Random(seed.hashCode)

  var branches: List[Branch] = Nil
  override def vertices = branches
  var connections: List[BranchConnection] = Nil

  private def rInt = rng.nextInt & Int.MaxValue
  private def rDouble = rng.nextDouble

  def randomBranch = branches(rInt % branches.size)

  // Branch positions 
  for (i <- 0 until branchCount) {
    var newBranch: Branch = null
    do {
      newBranch = Branch(Vec2(rInt % width, rInt % height), seed = rInt)
      newBranch.id = i
    } while (branches.exists(d =>
      (d distance newBranch) < minDistance) ||
      newBranch.x - branchDiameter / 2 < 0 ||
      newBranch.x + branchDiameter / 2 > width ||
      newBranch.y - branchDiameter / 2 < 0 ||
      newBranch.y + branchDiameter / 2 > height)
    branches ::= newBranch
  }

  val startBranch = randomBranch // choose branch with highest degree?

  // Connections (minimum spanning tree on complete graph)
  connections = minimumSpanningTree map (x => BranchConnection(x.vA.asInstanceOf[Branch], x.vB.asInstanceOf[Branch]))

  // More Connections (usually creating cycles and have restrictions)
  for (branch <- branches) {
    val d1 = branch
    val close = branches.sortBy(_ distance d1).tail zip edgeProbabilities
    val d2s = close.filter(rDouble <= _._2).map(_._1)
    for ((connection, d2) <- d2s.map(d2 => (BranchConnection(d1, d2), d2))) {
      if (!connections.exists(_.intersects(connection)) &&
        !branches.filterNot(d => d == connection.nA || d == connection.nB).exists { d => connection.line.segentDistance(d.point) < Config.minLineBranchDistance } &&
        d1.degree(connections) <= maxDegree && d2.degree(connections) <= maxDegree)
        connections ::= connection
    }
  }

  // Game path (choose the closest one possible)
  var gamePath: List[Branch] = List(startBranch)
  while (gamePath.size < branches.size) {
    val candidates = gamePath.flatMap(_.neighbours(connections)).distinct.diff(gamePath).asInstanceOf[List[Branch]]
    gamePath ::= candidates.minBy(_ distance gamePath.head)
  }
  for ((branch, i) <- gamePath.reverse zipWithIndex)
    branch.id = i

  def drawToImage(filename: String) {
    import java.io.File

    val backgroundColor = new Color(0xEEEEEE)
    val branchColor = new Color(0xCCCCCC)
    val startColor = new Color(0x00A020)
    val connectionColor = new Color(0x999999)
    val contourColor = new Color(0x666666)
    val textColor = new Color(0x333333)
    val textFont = new Font("Sans", Font.BOLD, 20)

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g: Graphics2D = image.createGraphics
    import g._
    setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)
    setStroke(new java.awt.BasicStroke(3))
    setFont(textFont)
    val frc = getFontRenderContext
    def stringBounds(s: String) = {
      val bounds = textFont.getStringBounds(s, frc)
      val metrics = textFont.getLineMetrics(s, frc)
      Vec2(bounds.getWidth, metrics.getHeight)
    }

    def fillCircle(x: Int, y: Int, diameter: Int) = fillOval(x - diameter / 2, y - diameter / 2, diameter, diameter)
    def drawCircle(x: Int, y: Int, diameter: Int) = drawOval(x - diameter / 2, y - diameter / 2, diameter, diameter)
    def drawBranch(branch: Branch, color: Color) = {
      setColor(color)
      fillCircle(branch.x, branch.y, branchDiameter)
      setColor(contourColor)
      drawCircle(branch.x, branch.y, branchDiameter)

      setColor(textColor)
      val string = branch.id.toString
      val bounds = stringBounds(string)
      drawString(string, (branch.x - bounds.x / 2).toInt, (branch.y + bounds.y / 2).toInt)
    }

    def drawBranchConnection(edge: BranchConnection, color: Color) {
      setColor(color)
      drawLine(edge.nA.x, edge.nA.y, edge.nB.x, edge.nB.y)
    }

    setBackground(backgroundColor)
    clearRect(0, 0, width, height)

    for (connection <- connections) {
      drawBranchConnection(connection, connectionColor)
    }

    for (branch <- branches)
      drawBranch(branch, branchColor)

    drawBranch(startBranch, startColor)

    val outputfile = new File(filename)
    ImageIO.write(image, "png", outputfile)
  }
}







