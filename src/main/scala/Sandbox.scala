import command.Commit.isATxtFile

import scala.util.matching.Regex

object Sandbox extends App() {

  val arrayPaths = Array(Array("hello", "world", "asma.txt"), Array("papa", "h.txt"), Array("khekhe.txt")
  )

  val newArborescence = arrayPaths.map(path => {
    if (isATxtFile(path.head)) path.tail
    else path
  })

  private def isATxtFile(fileName: String): Boolean = {
    val numPattern = new Regex("^([a-zA-Z0-9\\s_\\\\.\\-\\(\\):])+\\.(txt)$")
    numPattern.findAllIn(fileName).nonEmpty
  }

  println(newArborescence(2).size)
}