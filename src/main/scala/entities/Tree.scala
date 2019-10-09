package entities

import utils.Hash.encryptThisString

case class Tree(
                 treePath: String,
                 content: List[String]
               ){
  /**
   *
   * @return a String corresponding to the hash based on the content of the tree
   */
  def idTree(): String = {
    encryptThisString(content.mkString("/n"))
  }
}
