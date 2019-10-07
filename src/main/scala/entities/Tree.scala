package entities

case class Tree(
                 parentTree: String,
                 treeName: String,
                 listTrees: Array[String],
                 listBlobs: Array[String]
               )