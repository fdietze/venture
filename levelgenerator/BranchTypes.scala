package venture.branchtypes

import java.awt.Color

trait BranchType { def color:Color }
trait Sky extends BranchType
trait Ground extends BranchType
trait Underground extends BranchType
trait Surreal extends BranchType

trait Transition { def color:Color = new Color(0xAAAAAA) }

object Types {
	val cloud     = new Sky { def color = new Color(0xe9f4ff) }
	val skyIsland = new Sky { def color = new Color(0xbedeff) }
	val sky = cloud :: skyIsland :: Nil
	
	val lake     = new Ground { def color = new Color(0x49a4ff) }
	val city     = new Ground { def color = new Color(0x8d8d8d) }
	val forest   = new Ground { def color = new Color(0x007d43) }
	val mountain = new Ground { def color = new Color(0x5c5c5c) }
	val iceLand = new Ground { def color = new Color(0x36cfed) }
	val	ground = lake :: city :: forest :: mountain :: iceLand :: Nil
	
	val iceCave     = new Underground { def color = new Color(0x31687c) }
	val cave        = new Underground { def color = new Color(0x4c3121) }
	val lavaCave    = new Underground { def color = new Color(0xde6712) }
	val mine        = new Underground { def color = new Color(0x4e6b8a) }
	val floodedCave = new Underground { def color = new Color(0x093a6e) }
	val underground = cave :: lavaCave :: mine :: floodedCave :: iceCave :: Nil

/*
trait MirrorWorld extends Surreal
trait Hell extends Surreal
infinte libary
void
heaven
*/

/*
	val skyShip      = new Transition {}
	val direct       = new Transition {}
	val skyElevator  = new Transition {}
	val mineElevator = new Transition {}

	val transitions = Map[Set[BranchType], Set[Transition]](
		Set[BranchType](sky:_*) -> Set(direct),
		Set[BranchType](ground:_*) -> Set(direct),
		Set[BranchType](underground:_*) -> Set(direct),
		
		Set[BranchType](cloud, mountain) -> Set(skyShip),
		Set[BranchType](forest, cloud) -> Set(direct),
		Set[BranchType](mountain, skyIsland) -> Set(skyShip),
		Set[BranchType](mountain, iceCave) -> Set(direct),
		Set[BranchType](mountain, cave) -> Set(direct),
		Set[BranchType](mountain, lavaCave) -> Set(direct),
		Set[BranchType](mountain, mine) -> Set(direct),
		Set[BranchType](mountain, floodedCave) -> Set(direct),
		Set[BranchType](iceLand, iceCave) -> Set(direct),
		Set[BranchType](city, cloud)     -> Set(skyElevator),
		Set[BranchType](mountain, cloud) -> Set(skyElevator, skyShip),
		Set[BranchType](floodedCave, lake) -> Set(skyShip)
		
	)*/

}
