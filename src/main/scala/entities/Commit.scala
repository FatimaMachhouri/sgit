package entities

case class Commit (
                  commitHash: String,
                  commitParentHash: String,
                  date: String,
                  listDifferences: Map[String, List[String]], //between the commit and the parent commit
                  listNewFiles: List[String] //between the commit and the parent commit
                  )
