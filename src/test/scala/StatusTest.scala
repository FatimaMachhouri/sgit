import org.scalatest.{BeforeAndAfter, FlatSpec}
import command.{Add, Commit, Init, Status}

import scala.reflect.io.Directory
import java.io.File

import utils.FileIO.{createDirectory, createFile, getContentFile, writeInFile}
import utils.Path.getFilesDirectory


class StatusTest extends FlatSpec with BeforeAndAfter {

  before {
    val statusTestDirectory = new File(".").getCanonicalPath + File.separator + "StatusDirectoryTest"
    createDirectory(statusTestDirectory)
    Init.init(statusTestDirectory)

    createFile(statusTestDirectory + File.separator + "test.txt")
    createFile(statusTestDirectory + File.separator + "test1.txt")
    createFile(statusTestDirectory + File.separator + "test2.txt")
  }


  after {
    val statusTestDirectory = new File(".").getCanonicalPath + File.separator + "StatusDirectoryTest"
    val directory = new Directory(new File(statusTestDirectory))
    directory.deleteRecursively()
  }


  "Status" should "return the untracked files ie files present in the working tree but not in the stage" in {
    val statusTestDirectory = new File(".").getCanonicalPath + File.separator + "StatusDirectoryTest"

    Add.add(statusTestDirectory, statusTestDirectory, List(statusTestDirectory + File.separator + "test.txt")) //test.txt is tracked

    val stageContent = getContentFile(statusTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")
    val filesInCurrentDirectory = getFilesDirectory(statusTestDirectory)

    assert(Status.getUntrackedFiles(statusTestDirectory, stageContent, filesInCurrentDirectory).diff(List("test1.txt", "test2.txt")).isEmpty)
  }


  it should "return the modified files ie files present in the working tree and in the stage but associated to different content" in {
    val statusTestDirectory = new File(".").getCanonicalPath + File.separator + "StatusDirectoryTest"

    Add.add(statusTestDirectory, statusTestDirectory, List(statusTestDirectory + File.separator + "test.txt"))
    writeInFile(statusTestDirectory + File.separator + "test.txt", "test")

    val stageContent = getContentFile(statusTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")
    val filesInCurrentDirectory = getFilesDirectory(statusTestDirectory)

    assert(Status.getModifiedFiles(statusTestDirectory, stageContent, filesInCurrentDirectory) === Array("test.txt"))
  }


  it should "return the new files (not present in the last commit)" in {
    val statusTestDirectory = new File(".").getCanonicalPath + File.separator + "StatusDirectoryTest"

    Add.add(statusTestDirectory, statusTestDirectory, List(statusTestDirectory + File.separator + "test.txt"))
    Add.add(statusTestDirectory, statusTestDirectory, List(statusTestDirectory + File.separator + "test1.txt"))

    Commit.commit(statusTestDirectory)

    Add.add(statusTestDirectory, statusTestDirectory, List(statusTestDirectory + File.separator + "test2.txt"))

    val stageContent = getContentFile(statusTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")
    val commitStageContent = getContentFile(statusTestDirectory + File.separator + ".sgit" + File.separator + "STAGECOMMIT")

    assert(Status.getChangesToBeCommitted(statusTestDirectory, stageContent, commitStageContent) === Array("new file:   test2.txt"))
  }


  it should "return the modified files (different from the last commit)" in {
    val statusTestDirectory = new File(".").getCanonicalPath + File.separator + "StatusDirectoryTest"

    Add.add(statusTestDirectory, statusTestDirectory, List(statusTestDirectory + File.separator + "test.txt"))
    Add.add(statusTestDirectory, statusTestDirectory, List(statusTestDirectory + File.separator + "test1.txt"))

    Commit.commit(statusTestDirectory)

    writeInFile(statusTestDirectory + File.separator + "test1.txt", "test")
    Add.add(statusTestDirectory, statusTestDirectory, List(statusTestDirectory + File.separator + "test1.txt"))

    val stageContent = getContentFile(statusTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")
    val commitStageContent = getContentFile(statusTestDirectory + File.separator + ".sgit" + File.separator + "STAGECOMMIT")

    assert(Status.getChangesToBeCommitted(statusTestDirectory, stageContent, commitStageContent) === Array("modified:   test1.txt"))
  }


  it should "return the current branch" in {
    val statusTestDirectory = new File(".").getCanonicalPath + File.separator + "StatusDirectoryTest"
    assert(getContentFile(statusTestDirectory + File.separator + ".sgit" + File.separator + "HEAD") == "master")
  }

}