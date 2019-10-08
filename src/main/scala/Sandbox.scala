import java.io.File

import Sandbox.{createTree, isATxtFile}
import command.Commit.{createTree, formatStageFile, isATxtFile}
import entities.Tree

import scala.io.Source
import scala.util.matching.Regex

object Sandbox extends App() {


  def createTreePath(arrayPath: Array[String]): List[Tree] = {
    val arrayPaths = Array("hello", "world", "test" ,"asma.txt")

    def commitTailRec2(parentTree: String, treeName: String, arrayPaths: Array[String]): List[Tree] = {
      if (isATxtFile(arrayPaths.head)) return Nil
      else {
        createTree(parentTree, arrayPaths.head, Array(arrayPaths.tail.head)) :: commitTailRec2(arrayPaths.head, arrayPaths.head, arrayPaths.tail)
      }
    }

    val listTrees = commitTailRec2("origin", "o", arrayPaths)
    listTrees
    //listTrees.map(a=> println(a.parentTree + " -- " + a.treeName + " -- " + a.listBlobs.mkString("/") + " -- " + a.listTrees.mkString("/")))
  }


  def createTreesPath(): List[Tree] = {
    val arrayPaths = Array("hello/world/test.txt", "world/test1.txt", "test2.txt")

    def createTreesPathTailRec(parentTree: String, treeName: String, arrayPaths: Array[String], listTrees: List[Tree]): List[Tree] = {

      if (arrayPaths.isEmpty) {
        return Nil
      }
      else {
        //Step 1 : We create the tree associated to the path
        val currenTree = createTreePath(arrayPaths.head.split("/"))

        //Step 2 : We merge the arborescence created with the arborescence already existing
        val newListTrees = mergeListTrees(listTrees, currenTree)

        //Step 3 : We run the recursive algorithm on the next path, with the new list of trees
        return createTreesPathTailRec(parentTree, treeName, arrayPaths.tail, newListTrees)
      }

      createTreesPathTailRec("o", "origin", arrayPaths, List())

    }

    createTreesPathTailRec("o", "origin", arrayPaths, List())
    //listTrees.map(a=> println(a.parentTree + " -- " + a.treeName + " -- " + a.listBlobs.mkString("/") + " -- " + a.listTrees.mkString("/")))

  }


  def mergeListTrees(list1: List[Tree], list2: List[Tree]): List[Tree] = {

    def mergeListTreesTailRec(listTreeMerged: List[Tree], listToMerge: List[Tree]): List[Tree] = {
      if (listToMerge.isEmpty) return listTreeMerged
      else {
        val currentTreeToAdd = listToMerge.head

        //Step 1 : We look if the tree is already existing
        val found = listTreeMerged.exists(tree => tree.treeName == currentTreeToAdd.treeName && tree.parentTree == currentTreeToAdd.parentTree)

        //Step 2 : if not, we create a new tree and we run the recursive algorithm
        if (!found) mergeListTreesTailRec(listToMerge.head :: listTreeMerged, listToMerge.tail)
        else { //there is already a tree existing, so we merge
          val newListTreeMerged = listTreeMerged.map(tree => {
            if (tree.treeName == currentTreeToAdd.treeName && tree.parentTree == currentTreeToAdd.parentTree) new Tree(tree.parentTree, tree.treeName, tree.listTrees ++ currentTreeToAdd.listTrees, tree.listBlobs ++ currentTreeToAdd.listBlobs)
            else tree
          })
          mergeListTreesTailRec(newListTreeMerged, listToMerge.tail)
        }
      }
    }

    mergeListTreesTailRec(List(), list1 ++ list2)
  }

  val tree1 = Tree("tree1", "name1", Array("hello"), Array("a.txt"))
  val tree2 = Tree("tree2", "name2", Array("world"), Array())
  val tree3 = Tree("tree1", "name1", Array("test"), Array("c.txt"))

  //mergeListTrees(List(tree1, tree2), List(tree3)).map(t => println(t.parentTree + " " + t.treeName + " " + t.listTrees.mkString("/") + " " + t.listBlobs.mkString("/")))

  /*
  def commit(): Unit = {
    val arrayPaths = Array(Array("fatima.txt"), Array("hello", "world", "asma.txt"), Array("papa", "h.txt"))

    def commitTailRec(parentTree: String, treeName: String, arrayPaths: Array[Array[String]]): List[Tree] = {
      if (arrayPaths.length==0) return Nil
      else {
        if (isATxtFile(arrayPaths.head.head)) {
          createTree(parentTree, treeName, Array(arrayPaths.head.head)) :: commitTailRec(parentTree, treeName, arrayPaths.tail)
        }
        else { //I'm a directory

          val newHead = arrayPaths.head.tail
          val newTail = arrayPaths.tail
          val newArrayPaths = Array(newHead) ++ newTail
          createTree(parentTree, arrayPaths.head.head, Array(arrayPaths.head.tail(0))) :: commitTailRec(arrayPaths.head.head, arrayPaths.head.tail.head, newArrayPaths)
        }
      }

    }

    //val listTrees = commitTailRec("origin", "o", arrayPaths)
    //println(listTrees.size)
    //listTrees.map(a=> println(a.parentTree + " -- " + a.treeName + " -- " + a.listBlobs.mkString("/") + " -- " + a.listTrees.mkString("/")))

  }

  */

  private def isATxtFile(fileName: String): Boolean = {
    val numPattern = new Regex("^([a-zA-Z0-9\\s_\\\\.\\-\\(\\):])+\\.(txt)$")
    numPattern.findAllIn(fileName).nonEmpty
  }

  private def createTree(parentTree: String, treeName: String, components: Array[String]): Tree = {
    val trees = components.filter(!isATxtFile(_))
    val blobs = components.filter(isATxtFile(_))
    Tree(parentTree, treeName, trees, blobs)
  }

}