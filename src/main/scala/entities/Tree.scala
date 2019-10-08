package entities

import utils.Hash.encryptThisString

case class Tree(
                 parentTree: String,
                 treeName: String,
                 listTrees: Array[String],
                 listBlobs: Array[String]
               ) {

  /**
   *
   * @return a String corresponding to the hash based on the content of the tree
   */
  def idTree(): String = {
    val stringTrees = encryptThisString(listTrees.mkString(""))
    val stringBlobs = encryptThisString(listBlobs.mkString(""))
    encryptThisString(treeName+stringTrees+stringBlobs)
  }

  /**
   *
   * @return a boolean : true if the tree contains a subtree otherwise false
   */
  def containASubtTree(): Boolean = {
    listTrees.size != 0
  }

}