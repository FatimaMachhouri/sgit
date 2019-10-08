package command

import java.io.File
import entities.Tree
import scala.io.Source
import scala.util.matching.Regex

object Commit {

  /**
   *
   * @return a list of Tree which corresponds to the arborescence of the stafe file
   */
  def commit(): List[Tree] = {
    val arrayPaths = formatStageFile()

    def createTreesPathTailRec(parentTree: String, treeName: String, arrayPaths: Array[String], listTrees: List[Tree]): List[Tree] = {

      if (arrayPaths.isEmpty) {
        listTrees
      }
      else {
        //Step 1 : We create the tree associated to the path
        val currenTree = createTreePath(arrayPaths.head.split("/"))

        //Step 2 : We merge the arborescence created with the arborescence already existing
        val newListTrees = mergeListTrees(listTrees, currenTree)

        //Step 3 : We run the recursive algorithm on the next path, with the new list of trees
        createTreesPathTailRec(parentTree, treeName, arrayPaths.tail, newListTrees)
      }

    }

    createTreesPathTailRec("None", "origin", arrayPaths, List())
  }

  /**
   *
   * @param arrayPath
   * @return
   * Takes in parameter a path formatted as an Array and return the trees created by this path
   */
  private def createTreePath(arrayPath: Array[String]): List[Tree] = {

    def createTreePathTailRec(parentTree: String, treeName: String, arrayPaths: Array[String]): List[Tree] = {
      if (isATxtFile(arrayPaths.head)) return Nil
      else {
        createTree(parentTree, arrayPaths.head, Array(arrayPaths.tail.head)) :: createTreePathTailRec(arrayPaths.head, arrayPaths.head, arrayPaths.tail)
      }
    }

    createTreePathTailRec("None", "origin", arrayPath)
  }


  /**
   *
   * @param list1
   * @param list2
   * @return a list of Tree
   * Takes in parameter 2 lists and merge the second list in the first
   * Pre-condition : the list1 must not have duplicates
   * The result corresponds to the list1 + list2 without any duplicate and with the equal trees merged
   */
  private def mergeListTrees(list1: List[Tree], list2: List[Tree]): List[Tree] = {

    def mergeListTreesTailRec(listTreeMerged: List[Tree], listToMerge: List[Tree]): List[Tree] = {
      if (listToMerge.isEmpty) listTreeMerged
      else {
        val currentTreeToAdd = listToMerge.head

        //Step 1 : We look if the tree is already existing
        val found = listTreeMerged.exists(tree => tree.treeName == currentTreeToAdd.treeName && tree.parentTree == currentTreeToAdd.parentTree)

        //Step 2 : if not, we create a new tree and we run the recursive algorithm
        if (!found) mergeListTreesTailRec(listToMerge.head :: listTreeMerged, listToMerge.tail)
        else { //there is already a tree existing, so we merge
          val newListTreeMerged = listTreeMerged.map(tree => {
            if (tree.treeName == currentTreeToAdd.treeName && tree.parentTree == currentTreeToAdd.parentTree) new Tree(tree.parentTree, tree.treeName, (tree.listTrees ++ currentTreeToAdd.listTrees).distinct, (tree.listBlobs ++ currentTreeToAdd.listBlobs).distinct)
            else tree
          })
          mergeListTreesTailRec(newListTreeMerged, listToMerge.tail)
        }
      }
    }

    mergeListTreesTailRec(List(), list1 ++ list2)
  }


  /**
   *
   * @param parentTree
   * @param treeName
   * @param components
   * @return a Tree with the corresponding parameters
   */
  private def createTree(parentTree: String, treeName: String, components: Array[String]): Tree = {
    val trees = components.filter(!isATxtFile(_))
    val blobs = components.filter(isATxtFile(_))
    Tree(parentTree, treeName, trees, blobs)
  }


  /**
   *
   * @param fileName
   * @return True if the file is a txt else false
   */
  private def isATxtFile(fileName: String): Boolean = {
    val numPattern = new Regex("^([a-zA-Z0-9\\s_\\\\.\\-\\(\\):])+\\.(txt)$")
    numPattern.findAllIn(fileName).nonEmpty
  }


  /**
   *
   * @return
   *
   * Each array corresponds to a path and in each array we have the path which is splitted in an array
   * For exemple : a1/b1/file1 and a2/b2/file2 --> [ [a1, b1, file1], [a2, b2, file2] ]
   */
  private def formatStageFile(): Array[String] = {
    val currentRepositoryPath = new File(".").getCanonicalPath + File.separator + "s.git" + File.separator + "STAGE"

    //We retrive the STAGE file content
    val fileContent = Source.fromFile(currentRepositoryPath).mkString

    //We split to have an array in which each table box corresponds to a line of the STAGE file
    val fileContentTab = fileContent.split("\n")

    //We keep only the path and we delete the blod id
    Array("origin/") ++ fileContentTab.map(line => line.split(" ")(1))
  }

}
