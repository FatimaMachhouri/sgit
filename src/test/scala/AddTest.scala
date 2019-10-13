import java.io.File
import org.scalatest.{BeforeAndAfter, FlatSpec}
import utils.FileIO.{writeInFile, getContentFile}
import utils.Hash.encryptThisString
import scala.reflect.io.Directory
import command.{Init, Add}

class AddTest extends FlatSpec with BeforeAndAfter {

  before {
    val sgitPath = new File(".").getCanonicalPath + File.separator + ".sgit"
    val directory = new Directory(new File(sgitPath))
    directory.deleteRecursively()
  }


  after {
    val rootPath = new File(".").getCanonicalPath

    val sgitPath = rootPath + File.separator + ".sgit"
    val directory = new Directory(new File(sgitPath))
    directory.deleteRecursively()

    val filePath = rootPath + File.separator + "test.txt"
    val file = new Directory(new File(filePath))
    file.delete()
  }


  "Add" should "create a blob file in the Blobs directory" in {
    Init.init
    val rootPath = new File(".").getCanonicalPath
    val file = new File(rootPath + File.separator + "test.txt")
    file.createNewFile()

    Add.add(rootPath, rootPath, List(rootPath + File.separator + "test.txt"))

    val blobsDirectory = new File(rootPath + File.separator + ".sgit" + File.separator + "Blobs")
    assert(blobsDirectory.list.length == 1)
  }


  it should "create a blob which has the same content as the file" in {
    Init.init
    val rootPath = new File(".").getCanonicalPath
    val file = new File(rootPath + File.separator + "test.txt")
    file.createNewFile()
    writeInFile(rootPath + File.separator + "test.txt", "test content")

    Add.add(rootPath, rootPath, List(rootPath + File.separator + "test.txt"))

    val contentBlob = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + encryptThisString("test content"))

    assert(contentBlob == getContentFile(rootPath + File.separator + "test.txt"))
  }


  it should "add the blob created to the stage file if it isn't already staged" in {
    Init.init
    val rootPath = new File(".").getCanonicalPath
    val file = new File(rootPath + File.separator + "test.txt")
    file.createNewFile()

    Add.add(rootPath, rootPath, List(rootPath + File.separator + "test.txt"))

    val contentStage = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")

    assert(contentStage.split("\n").length == 1)
  }


  it should "not add the blob created to the stage file if it is already staged" in {
    Init.init
    val rootPath = new File(".").getCanonicalPath
    val file = new File(rootPath + File.separator + "test.txt")
    file.createNewFile()

    Add.add(rootPath, rootPath, List(rootPath + File.separator + "test.txt"))
    Add.add(rootPath, rootPath, List(rootPath + File.separator + "test.txt"))

    val contentStage = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")

    assert(contentStage.split("\n").length == 1)
  }

}