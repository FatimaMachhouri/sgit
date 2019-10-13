import org.scalatest.{BeforeAndAfter, FlatSpec}
import command.Init
import scala.reflect.io.Directory
import java.io.File

class InitTest extends FlatSpec with BeforeAndAfter {

  before {
    val sgitPath = new File(".").getCanonicalPath + File.separator + ".sgit"
    val directory = new Directory(new File(sgitPath))
    directory.deleteRecursively()
  }


  after {
    val sgitPath = new File(".").getCanonicalPath + File.separator + ".sgit"
    val directory = new Directory(new File(sgitPath))
    directory.deleteRecursively()
  }


  "Init" should "create a .sgit in the path" in {
    val init = Init
    assert(init.init)
  }


  it should "not create a .sgit if already exists in the path" in {
    val init = Init
    init.init
    assert(!init.init)
  }

}