package venture.dungeonGenerator

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Color
import java.awt.Font
import javax.imageio.ImageIO

import graph._
import geometry._

import venture.branchtypes._

import collection.mutable

object Config {
	val width = 800
	val height = 600
	val branchCount = Array(3, 4, 7) //TODO: muss bei zu hoher Zahl trotzdem terminieren
	val skyBranchGroundDistance = 100
	val undergroundBranchGroundDistance = 50
	
	val maxDegree = 4
	val minDistance = 90
	val minBorderDistance = minDistance / 2
	val edgeProbabilities = Array(0.9, 0.6, 0.3)
	val minLineBranchDistance = 90 //TODO: statt Pixelabstand, den Winkel vom erzeugten Dreieck limitieren
	val frameThickness = 10
	
	val itemDependencies = 0 //branchCount.sum / 3
}



case class Branch(_point: Vec2, seed: Int) extends EuclideanVertex(_point) {
	def x = point.x.toInt
	def y = point.y.toInt
	var branchType:BranchType = null
	var id: Int = 0
	var bounds:ConvexPolygon = null
	override def toString = "Branch(%d: %s)" format (id, point)
}

case class BranchConnection(nA: Branch, nB: Branch) extends EuclideanEdge(nA, nB) {
	def lowerId = if( nA.id < nB.id ) nA else nB
	def higherId = if( nA.id > nB.id ) nA else nB
	def midPoint = lineSegment.midPoint
}

case class ItemDependency(branch:Branch, connection:BranchConnection)




class Dungeon(seed: Any) extends EuclideanGraph {
	import Config._
  
  import venture.branchGenerator.BranchProperties
  def toBranchProperties:List[BranchProperties] = {
    import venture.branchGenerator._
    val properties = new mutable.ListBuffer[BranchProperties]
    for( branch <- branches ) {
      val neighbours = new mutable.ListBuffer[BranchNeighbourInfo]
      for( neighbourBranch <- branch.neighbours(connections).map(_.asInstanceOf[Branch]) ) {
        neighbours += BranchNeighbourInfo(
          branchType = neighbourBranch.branchType,
          relativePosition = neighbourBranch.point - branch.point,
          requiredItems = Nil
        )
      }
    
      properties += BranchProperties(
        seed = branch.seed,
        branchType = branch.branchType,
        difficulty = branch.id,
        bounds = branch.bounds - branch.point,
        neighbours = neighbours.toList,
        requiredItems = Nil,
        containedItems = Nil
      )
    }
    
    properties.toList
  }
  
  
	private val rng = new util.Random(seed.hashCode)

	var skyBranches: List[Branch] = Nil
	var groundBranches: List[Branch] = Nil
	var undergroundBranches: List[Branch] = Nil
	def branches = skyBranches ::: groundBranches ::: undergroundBranches

	override def vertices = branches
	var connections: List[BranchConnection] = Nil

	private def rInt = rng.nextInt & Int.MaxValue
	private def rDouble = rng.nextDouble

	def randomBranch = branches(rInt % branches.size)

	def branchTooClose(branch:Branch) = {
		branches.exists(d => (d distance branch) < minDistance) ||
		branch.x - minBorderDistance < 0 ||
		branch.x + minBorderDistance > width ||
		branch.y - minBorderDistance < 0 ||
		branch.y + minBorderDistance > height
	}



	val groundLineSeed = rInt
	val groundLine = (x:Int) => (noise.Noise.fractalNoise1(groundLineSeed,x.toDouble/(width/2),2,2.0)*height/6+height/2).toInt


	// Ground Branches (on GroundLine)
	for (i <- 0 until branchCount(1)) {
		var newBranch: Branch = null
		do {
			val x = rInt % width
			newBranch = Branch(Vec2(x, groundLine(x)), seed = rInt)
			newBranch.id = i
		} while ( branchTooClose(newBranch) )
		newBranch.branchType = Types.ground(rInt % Types.ground.size)
		
		groundBranches ::= newBranch
	}

	// Sky Branches (above GroundLine)
	for (i <- 0 until branchCount(0)) {
		var newBranch: Branch = null
		do {
			val x = rInt % width
			newBranch = Branch(Vec2(x, rInt % (groundLine(x) - skyBranchGroundDistance)), seed = rInt)
			newBranch.id = i
		} while ( branchTooClose(newBranch) )
		newBranch.branchType = Types.sky(rInt % Types.sky.size)
		
		skyBranches ::= newBranch
	}

	// Underground Branches (below GroundLine)
	for (i <- 0 until branchCount(2)) {
		var newBranch: Branch = null
		do {
			val x = rInt % width
			newBranch = Branch(Vec2(x, groundLine(x) + undergroundBranchGroundDistance + (rInt % (height - groundLine(x) - undergroundBranchGroundDistance))), seed = rInt)
			newBranch.id = i
		} while ( branchTooClose(newBranch) )
		newBranch.branchType = Types.underground(rInt % Types.underground.size)
		
		undergroundBranches ::= newBranch
	}

  val bounds = Rectangle(Vec2(frameThickness,frameThickness), Vec2(width-frameThickness, height-frameThickness))
  val voronoi = voronoiDiagram(bounds)
  for( branch <- branches ) {
    branch.bounds = voronoi(branch)
  }

	val startBranch = randomBranch // TODO: choose branch with highest degree?

	// base connections (minimum spanning tree on complete graph)
	connections = minimumSpanningTree map (x => BranchConnection(x.vA.asInstanceOf[Branch], x.vB.asInstanceOf[Branch]))


	// More Connections (usually creating cycles and have restrictions)
	for (branch <- branches) {
		val d1 = branch
		val close = branches.sortBy(_ distance d1).tail zip edgeProbabilities
		val d2s = close.filter(rDouble <= _._2).map(_._1)
		for ((connection, d2) <- d2s.map(d2 => (BranchConnection(d1, d2), d2))) {
			if (!connections.exists(_.intersects(connection)) &&
				!branches.filterNot(d => d == connection.nA || d == connection.nB).exists { d => connection.lineSegment.distance(d.point) < Config.minLineBranchDistance } &&
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
		branch.id = i+1


  // Item dependencies
  val items = rng.shuffle(gamePath.tail).take(itemDependencies)
  val dependencies = items.map{ branch =>
    val candidates = connections.filter( _.higherId.id > branch.id )
    //TODO: also add dependencies to all edges between this branch and its neighbours
    ItemDependency(branch, candidates(rInt % candidates.size))
  }
  


	def drawToImage(filename: String) {
		import java.io.File

		val backgroundColor = new Color(0xEEEEEE)
		val connectionColor = new Color(0x999999)
		val contourColor = new Color(0x333333)
		val lightTextColor = new Color(0xFFFFFF)
		val darkTextColor = new Color(0x222222)
		val noiseLineColor = new Color(0x5ea264)
		val dependencyColor = new Color(0x22df81)
		val textFont = new Font("Sans", Font.BOLD, 20)
		val branchRadius = 20
		val strokeWidth = 3
		val antialiasing = true

		val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
		val g: Graphics2D = image.createGraphics
		import g._
		if( antialiasing ) {
		  setRenderingHint(RenderingHints.KEY_ANTIALIASING,
			  RenderingHints.VALUE_ANTIALIAS_ON)
		}
		setStroke(new java.awt.BasicStroke(strokeWidth))
		setFont(textFont)
		val frc = getFontRenderContext
		def stringBounds(s: String) = {
			val bounds = textFont.getStringBounds(s, frc)
			val metrics = textFont.getLineMetrics(s, frc)
			Vec2(bounds.getWidth, metrics.getHeight)
		}

		def fillCircle(x: Int, y: Int, radius: Int) = fillOval(x - radius, y - radius, radius * 2, radius * 2)
		def drawCircle(x: Int, y: Int, radius: Int) = drawOval(x - radius, y - radius, radius * 2, radius * 2)

		def drawNoiseLine( f:Int => Int ) {
			val xpoints = (0 until width).toArray
			val ypoints = xpoints map f
			setColor( noiseLineColor )
			drawPolyline(xpoints,ypoints,width)
		}
		
		def drawConvexPolygon(polygon:ConvexPolygonLike, color:Color) {
      val xpoints = polygon.map(_.x.toInt).toArray
      val ypoints = polygon.map(_.y.toInt).toArray
      setColor(color)
      drawPolygon(xpoints,ypoints,polygon.size)
		}

		def fillConvexPolygon(polygon:ConvexPolygonLike, color:Color) {
      val xpoints = polygon.map(_.x.toInt).toArray
      val ypoints = polygon.map(_.y.toInt).toArray
      setColor(color)
      fillPolygon(xpoints,ypoints,polygon.size)
		}
		
		def drawBranch(branch: Branch, color: Color) = {
			setColor(color)
			fillCircle(branch.x, branch.y, branchRadius)
			setColor(contourColor)
			drawCircle(branch.x, branch.y, branchRadius)
			
			val v = color.getComponents(new Array[Float](4)).take(3).max
			setColor(if( v > 0.5 ) darkTextColor else lightTextColor)
			val string = branch.id.toString
			val bounds = stringBounds(string)
			drawString(string, (branch.x - bounds.x / 2).toInt, (branch.y + bounds.y / 2).toInt)
		}

		def drawBranchPolygon(poly:ConvexPolygon, color: Color) = {
      fillConvexPolygon(poly, new Color(color.getRed, color.getGreen, color.getBlue, 40))
      drawConvexPolygon(poly, new Color(color.getRed, color.getGreen, color.getBlue, 100).darker.darker)
		}

		def drawBranchConnection(edge: BranchConnection, color: Color) {
			setColor(color)
			drawLine(edge.nA.x, edge.nA.y, edge.nB.x, edge.nB.y)
		}

		def drawItemDependency(dep: ItemDependency) {
			setColor(dependencyColor)
			drawLine(dep.branch.x, dep.branch.y, dep.connection.midPoint.x.toInt, dep.connection.midPoint.y.toInt)
		}


    // Start drawing
		setBackground(backgroundColor)
		clearRect(0, 0, width, height)
		
		// Ground line
		drawNoiseLine(groundLine)

    // Branch cells
    for( branch <- branches ) {
      val smallerPolygon = branch.bounds.smaller(branch.point, 0.9)
			drawBranchPolygon(smallerPolygon, branch.branchType.color)
    }

    // Branch connections
		for (connection <- connections) {
			drawBranchConnection(connection, connectionColor)
		}

    // Item dependencies
    for( dependency <- dependencies ) {
      drawItemDependency(dependency)
    }

    // Branch Circles
    for( branch <- branches ) {
			drawBranch(branch, branch.branchType.color)
    }

    // Start Branch
		setColor(contourColor)
		drawCircle(startBranch.x, startBranch.y, (branchRadius*1.4).toInt)

		val outputfile = new File(filename)
		ImageIO.write(image, "png", outputfile)
	}
}



