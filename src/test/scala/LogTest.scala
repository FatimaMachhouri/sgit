import org.scalatest.{BeforeAndAfter, FlatSpec}
import command.{Init, Add, Commit, Log}
import scala.reflect.io.Directory
import java.io.File
import utils.FileIO.{createDirectory, createFile, writeInFile}
import utils.Path.getFilesDirectory
import utils.FileIO.getContentFile

class LogTest extends FlatSpec with BeforeAndAfter {

  before {
    val logTestDirectory = new File(".").getCanonicalPath + File.separator + "LogDirectoryTest"
    createDirectory(logTestDirectory)
    Init.init(logTestDirectory)
  }


  after {
    val logTestDirectory = new File(".").getCanonicalPath + File.separator + "LogDirectoryTest"
    val directory = new Directory(new File(logTestDirectory))
    directory.deleteRecursively()
  }


  "Log" should "returns the new files between 2 commits" in {
    val logTestDirectory = new File(".").getCanonicalPath + File.separator + "LogDirectoryTest"

    createFile(logTestDirectory + File.separator + "fileCommit1.txt")
    Add.add(logTestDirectory, logTestDirectory, List(logTestDirectory + File.separator + "fileCommit1.txt"))
    Commit.commit(logTestDirectory)
    val commit1 = getFilesDirectory(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits")(0)

    createFile(logTestDirectory + File.separator + "fileCommit2.txt")
    Add.add(logTestDirectory, logTestDirectory, List(logTestDirectory + File.separator + "fileCommit2.txt"))
    Commit.commit(logTestDirectory)
    val commit2 = getFilesDirectory(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits").filter(elem => elem != commit1)(0)

    assert(Log.newFiles(logTestDirectory,
                        commit1.replace(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator, ""),
                        commit2.replace(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator, "")).length == 1)
  }


  it should "returns the files modified between 2 commits with their lists of differences" in {
    val logTestDirectory = new File(".").getCanonicalPath + File.separator + "LogDirectoryTest"

    createFile(logTestDirectory + File.separator + "fileCommit1.txt")
    writeInFile(logTestDirectory + File.separator + "fileCommit1.txt", "line1\nline3")
    Add.add(logTestDirectory, logTestDirectory, List(logTestDirectory + File.separator + "fileCommit1.txt"))
    Commit.commit(logTestDirectory)

    val commit1 = getFilesDirectory(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits")(0)

    writeInFile(logTestDirectory + File.separator + "fileCommit1.txt", "line1\nline2")
    Add.add(logTestDirectory, logTestDirectory, List(logTestDirectory + File.separator + "fileCommit1.txt"))
    Commit.commit(logTestDirectory)

    val commit2 = getFilesDirectory(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits").filter(elem => elem != commit1)(0)

    assert(Log.diffCommits(logTestDirectory,
                            commit1.replace(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator, ""),
                            commit2.replace(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator, ""))
               == Map("fileCommit1.txt" -> List("- line3", "+ line2")))
  }


  it should "not return the new empty files" in {
    val logTestDirectory = new File(".").getCanonicalPath + File.separator + "LogDirectoryTest"

    createFile(logTestDirectory + File.separator + "fileCommit1.txt")
    Add.add(logTestDirectory, logTestDirectory, List(logTestDirectory + File.separator + "fileCommit1.txt"))
    Commit.commit(logTestDirectory)

    val commit1 = getFilesDirectory(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits")(0)

    createFile(logTestDirectory + File.separator + "fileCommit2.txt")
    writeInFile(logTestDirectory + File.separator + "fileCommit2.txt", "")
    Add.add(logTestDirectory, logTestDirectory, List(logTestDirectory + File.separator + "fileCommit2.txt"))
    Commit.commit(logTestDirectory)

    val commit2 = getFilesDirectory(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits").filter(elem => elem != commit1)(0)

    assert(Log.diffCommits(logTestDirectory,
      commit1.replace(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator, ""),
      commit2.replace(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator, "")).isEmpty
    )
  }


  it should "return the non-empty new files with their content marked by a plus (+)" in {
    val logTestDirectory = new File(".").getCanonicalPath + File.separator + "LogDirectoryTest"

    createFile(logTestDirectory + File.separator + "fileCommit1.txt")
    Add.add(logTestDirectory, logTestDirectory, List(logTestDirectory + File.separator + "fileCommit1.txt"))
    Commit.commit(logTestDirectory)

    val commit1 = getFilesDirectory(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits")(0)

    createFile(logTestDirectory + File.separator + "fileCommit2.txt")
    writeInFile(logTestDirectory + File.separator + "fileCommit2.txt", "line1\nline2")
    Add.add(logTestDirectory, logTestDirectory, List(logTestDirectory + File.separator + "fileCommit2.txt"))
    Commit.commit(logTestDirectory)

    val commit2 = getFilesDirectory(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits").filter(elem => elem != commit1)(0)

    assert(Log.diffCommits(logTestDirectory,
      commit1.replace(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator, ""),
      commit2.replace(logTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator, ""))
      == Map("fileCommit2.txt" -> List("+ line1", "+ line2")))
  }


}