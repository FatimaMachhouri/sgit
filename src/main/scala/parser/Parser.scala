import command.{Add, Init}

object Parser extends App {

  parser(args)

  def parser(args: Array[String]): Unit = {

    args match {
      //INIT
      case Array("init", _) => {
        println("hello")
        init(args(1))
      }

      //ADD
      case Array("add", _) => add(args(1), args.tail.toList)
    }

    def init(path: String): Unit = {
      val i = new Init
      i.init(path)
    }

    def add(path: String, listFiles: List[String]): Unit = {
      val a = new Add
      a.add(path, listFiles)
    }
  }


}