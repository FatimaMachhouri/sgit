import java.io.File
import org.scalatest.{BeforeAndAfter, FlatSpec}
import utils.FileIO.{createDirectory, getContentFile, writeInFile, createFile}
import utils.Hash.encryptThisString
import scala.reflect.io.Directory
import command.{Add, Init}
import utils.Path.getFilesDirectory

class AddTest extends FlatSpec with BeforeAndAfter {

  before {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"
    createDirectory(addTestDirectory)
    createDirectory(addTestDirectory + File.separator + "DirectoryTest")
    createFile(addTestDirectory + File.separator + "test.txt")
    createFile(addTestDirectory + File.separator + "test1.txt")
    createFile(addTestDirectory + File.separator + "DirectoryTest" + File.separator + "test2.txt")

    Init.init(addTestDirectory)
  }


  after {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"
    val directory = new Directory(new File(addTestDirectory))
    directory.deleteRecursively()
  }


  "Add" should "create a blob file in the Blobs directory" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))

    val blobsDirectory = new File(addTestDirectory + File.separator + ".sgit" + File.separator + "Blobs")
    assert(blobsDirectory.list.length.equals(1))
  }


  it should "create a blob which has the same content as the file" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"
    writeInFile(addTestDirectory + File.separator + "test.txt", "test content")

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))

    val contentBlob = getContentFile(addTestDirectory + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + encryptThisString("test content"))
    assert(contentBlob.equals(getContentFile(addTestDirectory + File.separator + "test.txt")))
  }


  it should "create a blob named after the hash of the file content" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"
    writeInFile(addTestDirectory + File.separator + "test.txt", "test content")

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))

    val cryptedName = encryptThisString("test content")
    val blobName = getFilesDirectory(addTestDirectory + File.separator + ".sgit" + File.separator + "Blobs")(0).replace(addTestDirectory + File.separator + ".sgit" + File.separator + "Blobs" + File.separator, "")

    assert(blobName.equals(cryptedName))
  }


  it should "create as many files as different contents" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"
    writeInFile(addTestDirectory + File.separator + "test.txt", "test content")
    writeInFile(addTestDirectory + File.separator + "test1.txt", "test content1")

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt", addTestDirectory + File.separator + "test1.txt"))

    val blobsDirectory = new File(addTestDirectory + File.separator + ".sgit" + File.separator + "Blobs")
    assert(blobsDirectory.list.length.equals(2))
  }


  it should "create only 1 blob if 2 files have the same content " in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"
    writeInFile(addTestDirectory + File.separator + "test.txt", "test content")
    writeInFile(addTestDirectory + File.separator + "test1.txt", "test content")

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt", addTestDirectory + File.separator + "test1.txt"))

    val blobsDirectory = new File(addTestDirectory + File.separator + ".sgit" + File.separator + "Blobs")
    assert(blobsDirectory.list.length.equals(1))
  }


  it should "add the blob created to the stage file if it isn't already staged" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))
    val contentStage = getContentFile(addTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")

    assert(contentStage.nonEmpty)
  }


  it should "not add the blob created to the stage file if it is already staged" in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))
    val contentStageBefore = getContentFile(addTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "test.txt"))
    val contentStageAfter = getContentFile(addTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")

    assert(contentStageBefore.equals(contentStageAfter))
  }


  it should "add all the content of the directory (even the sub-files) when we use regex ." in {
    val addTestDirectory = new File(".").getCanonicalPath + File.separator + "AddDirectoryTest"

    Add.add(addTestDirectory, addTestDirectory, List(addTestDirectory + File.separator + "."))

    val contentStage = getContentFile(addTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")
    assert(contentStage.split("\n").length.equals(3))
  }

}