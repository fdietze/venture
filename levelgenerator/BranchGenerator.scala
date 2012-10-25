package venture.branchGenerator

import geometry.Vec2

case class Item(name: String)
case class BranchType(name: String)

case class BranchNeighbourInfo(
	branchType: BranchType,
	relativePosition: Vec2,
	requiredItems: List[Item])

case class BranchProperties(
	seed: Int,
	difficulty: Int,
	neighbours: List[BranchNeighbourInfo],
	requiredItems: List[Item],
	containedItems: List[Item])