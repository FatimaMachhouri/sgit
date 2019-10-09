package utils

import java.io.{BufferedWriter, File, FileWriter}

import entities.Tree

object FileIO {
  def createTrees(listTrees: List[Tree]): List[String] = {
    //We get the path
    val currentRepositoryPath = new File(".").getCanonicalPath
    val pathTrees = currentRepositoryPath + File.separator + ".sgit" + File.separator + "Trees"

    listTrees.map(tree => {
      //We create the tree file based on the crypted content of the tree
      val treeFile = new File(pathTrees + File.separator + tree.idTree())
      treeFile.createNewFile()

      //We write in the file the content of the tree
      val bw = new BufferedWriter(new FileWriter(pathTrees + File.separator + tree.idTree()))
      bw.write(tree.content.mkString("\n"))
      bw.close()
    })

    listTrees.map(tree => {
      val treePathTab = tree.treePath.split(File.separator)
      val newTreePathTab = treePathTab.dropRight(1)
      "Tree " + tree.idTree() + " " + newTreePathTab.mkString(File.separator)
    })

  }
}
