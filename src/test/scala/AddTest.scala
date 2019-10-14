import java.io.File
import org.scalatest.{BeforeAndAfter, FlatSpec}
import utils.FileIO.{createDirectory, getContentFile, writeInFile}
import utils.Hash.encryptThisString
import scala.reflect.io.Directory
import command.{Add, Init}

class AddTest extends FlatSpec with BeforeAndAfter {

  before {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"
    createDirectory(addTestDirectory)
    Init.init(addTestDirectory)
  }


  after {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"
    val directory = new Directory(new File(addTestDirectory))
    directory.deleteRecursively()
  }


  "Add" should "create a blob file in the Blobs directory" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"

    val file = new File(addTestDirectory + File.separator + "test.txt")
    file.createNewFile()

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))

    val blobsDirectory = new File(addTestDirectory + File.separator + ".sgit" + File.separator + "Blobs")
    assert(blobsDirectory.list.length == 1)
  }


  it should "create a blob which has the same content as the file" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"

    val file = new File(addTestDirectory + File.separator + "test.txt")
    file.createNewFile()
    writeInFile(addTestDirectory + File.separator + "test.txt", "test content")

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))

    val contentBlob = getContentFile(addTestDirectory + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + encryptThisString("test content"))

    assert(contentBlob == getContentFile(addTestDirectory + File.separator + "test.txt"))
  }


  it should "add the blob created to the stage file if it isn't already staged" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"

    val file = new File(addTestDirectory + File.separator + "test.txt")
    file.createNewFile()

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))

    val contentStage = getContentFile(addTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")

    assert(contentStage.split("\n").length == 1)
  }


  it should "not add the blob created to the stage file if it is already staged" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"

    val file = new File(addTestDirectory + File.separator + "test.txt")
    file.createNewFile()

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))
    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))

    val contentStage = getContentFile(addTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")

    assert(contentStage.split("\n").length == 1)
  }

}