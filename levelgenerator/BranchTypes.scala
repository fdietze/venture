package venture.branchtypes

import java.awt.Color

trait BranchType {
	def color:Color
}

trait Sky extends BranchType
trait Ground extends BranchType
trait Underground extends BranchType
trait Surreal extends BranchType

object Foo {

	val cloud = new Sky {
		def color = new Color(0x2266AA)
	}

/*trait SkyIsland extends Sky

trait Lake extends Ground
trait City extends Ground
trait Forest extends Ground
trait Mountain extends Ground

trait Cave extends Underground
trait LavaCave extends Underground
trait Mine extends Underground
trait FloodedCave extends Underground

trait MirrorWorld extends Surreal
trait Hell extends Surreal

trait Transition
trait SkyShip extends Transition
trait Direct extends Transition
trait SkyElevator extends Transition
trait MineElevator extends Transition


	val skyTypes = List(classOf[Cloud],classOf[SkyIsland])
	val groundTypes = List(classOf[Lake],classOf[City],classOf[Forest],classOf[Mountain])
	val undergroundType = List(classOf[Cave], classOf[LavaCave], classOf[Mine], classOf[FloodedCave])
*/
	/*
	val transitionCreationWeights = Map[Set[BranchType], Set[Transition]](
		Set[BranchType](City,Forest)     -> Set(Direct),
		Set[BranchType](Cloud)           -> Set(SkyShip),
		Set[BranchType](Cloud, Mountain) -> Set(SkyShip),
		Set[BranchType](City, Cloud)     -> Set(SkyElevator),
		Set[BranchType](Mountain, Cloud) -> Set(SkyElevator, SkyShip)
	)
	*/
}

