package command

import java.io.File

import scala.io.Source

object Commit3 {

  /**
   *
   * @return
   */
  def commit(): List[String] = {
    //We get the stage content
    val stage = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "STAGE"
    val stageContent = Source.fromFile(stage).mkString

    //We split in order to have each line of the stage in a box. A line has the form : Blob hash path
    val currentStage = stageContent.split("\n").toList

    def commitTailRec(currentStage: List[String]): List[String] = {
      if (areAllOriginChildren(currentStage)) {
        currentStage
      }
      else {
        //Step 1 :
        val deepest = deepestTrees(currentStage)

        //Step 2 :
        val deepestTreesMerged = merge(deepest)

        //Step 3 :
        val create = createTrees(deepestTreesMerged)

        //Step 4 :
        List()
      }
    }

    commitTailRec(currentStage)
  }

  private def deepestTrees(listPath: List[String]): List[String] = {
    List()
  }

  private def merge(listPath: List[String]): List[Map[String, String]] = {
    List()
  }

  private def createTrees(listPath: List[Map[String, String]]): List[String] = {
    List()
  }

  private def areAllOriginChildren(listPath: List[String]): Boolean = {
    true
  }

}
