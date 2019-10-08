package command

import java.io.{BufferedWriter, File, FileWriter}

import entities.Tree

object Commit2 {

  /*
  def outputArborescence(listTrees: List[Tree]): Unit = {
    if (listTrees.size==0) {}
    else {
      val currentTree = listTrees.head
      //If the tree doesn't contain any subtree, we can create it
      if (!currentTree.containASubtTree()) {
        val contentTree = contentTree(currentTree)
        val pathTrees = new File(".").getCanonicalPath + File.separator + ".sgit/Trees"
        val tree = new File(pathTrees + File.separator + currentTree.idTree())
        tree.createNewFile()
        val bw = new BufferedWriter(new FileWriter(tree))
        bw.write(contentTree)
        bw.close()
      }

      else {

      }

    }

  }
  */


  /**
   *
   * @param tree
   * @return the content of a tree in a String
   */
  def contentTree(tree: Tree): String = {
    val blobs = tree.listBlobs.mkString("\n")
    val trees = tree.listTrees.mkString("\n")

    tree.treeName + "\n" + trees + blobs
  }

/*
  /**
   *
   * @param list
   * @return a list of trees ordered from the deepest to least deep
   */
  def orderTree(list: List[Tree]): List[Tree] = {

    def orderTreeTailRec(listTrees: List[Tree], listTreesCreated: List[Tree]): Unit = {

      val deepestTrees = listTrees.map(tree => {
        if (!tree.containASubtTree()) tree
        else if (listTreesCreated.contains(tree.listTrees)) tree
      })

      orderTreeTailRec(listTrees.diff(deepestTrees), listTrees++deepestTrees)

    }

  }*/
}
