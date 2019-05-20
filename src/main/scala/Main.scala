import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val allLinesOfFile : List[String] = Source.fromResource("fens.txt").getLines.toList
    val listOfFens: List[FenStruct] = allLinesOfFile.flatMap(FenStruct.parse)

    listOfFens.foreach(_.printAll)

  }
}
