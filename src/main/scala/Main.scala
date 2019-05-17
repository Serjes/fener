import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
//    val stream: InputStream = getClass.getResourceAsStream("/fens.txt")
//    val lines: Iterator[String] = scala.io.Source.fromInputStream( stream ).getLines
    val readmeText : Iterator[String] = Source.fromResource("fens.txt").getLines
    readmeText.foreach(FenStruct.parse)

  }
}
