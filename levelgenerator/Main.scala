package venture

import java.io.File

object Main extends App {

  (new File("out/dungeons")).mkdirs()
  for (i <- 0 until 10) {
    println("Generating Dungeon %3d..." format i)
    val dungeon = new dungeonGenerator.Dungeon(seed = i)
    dungeon.drawToImage("out/dungeons/test%03d.png" format i)
    //println(dungeon.toBranchProperties.mkString("\n\n"))
  }

  println("done")
}
