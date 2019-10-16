package parser

case class Config(
                   mode: String = "",
                   path: String = "",
                   files: List[String] = List(),
                   arg: String = "",
                   patch: Boolean = false,
                   stat: Boolean = false,
                   name: String = "",
                   displayAll: Boolean = false,
                   verbose: Boolean = false
                 )