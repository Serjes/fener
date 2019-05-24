import scala.io.Source

object Main {
  //  val inputFileWithFens = "fens_with_error.txt"
  val inputFileWithFens = "fens_with_moves.txt"
  val inputFileWithFensAnswers = "fens_with_moves_answers.txt"
  //  val inputFileWithFens = "fens.txt"

  def main(args: Array[String]): Unit = {
    val listOfFens: List[FenStruct] = getInputFens(inputFileWithFens)
    val listOfAnswers: List[FenStruct] = getInputFens(inputFileWithFensAnswers)

        println("Дано: ")
        listOfFens.foreach(_.printAllFields)
        println("Надо: ")
        listOfAnswers.foreach(_.printAllFields)

        println("Обработаем ходы(берем fen-ы только с ходами):")
        val doubleList = listOfFens.filter(_.nextMove != null).zip(listOfAnswers)
        val counter = 0
        makeMoves(doubleList,counter)

//            listOfFens.foreach(_.drawBoard)
//    listOfFens(2).printAllFields
//    listOfFens(2).drawBoard
//    listOfFens(2).makeMove.printAllFields

  }

  private def makeMoves(fensAndAnswers: List[(FenStruct, FenStruct)], counter: Int): Unit = {
    val counter1 = counter + 1
    fensAndAnswers match {
      case head :: tail => {
        println(s"\n$counter1) Выполняем ход:")
        head._1.makeMove.printAllFields
//        head._1.printAllFields
        println("Сравните с правильным ответом:")
        head._2.printAllFields
        if (head._1.whoMove.sameElements(head._2.whoMove)
          && head._1.castling.sameElements(head._2.castling)
          && head._1.brokenField.sameElements(head._2.brokenField)
          //          && head._1.halfMoves.sameElements(head._2.halfMoves)
          && head._1.moves.sameElements(head._2.moves)
          && head._1.nextMove.sameElements(head._2.nextMove)
          && eq(head._1.fields, head._2.fields)
        ) println("Совпадают")
        else println("Несовпадают!!!")
        head._1.drawBoard
        makeMoves(tail, counter1)
      }
      case Nil => println("Все обработаны")
    }
  }

  private def eq(arr1: Array[Array[Char]], arr2: Array[Array[Char]]): Boolean = {
    var res = true
    for (i <- arr1.indices) res &= arr1(i).sameElements(arr2(i))
    res
  }

  private def getInputFens(fileName: String) = {
    val allLinesOfFile: List[String] = Source.fromResource(fileName).getLines.toList
    allLinesOfFile.flatMap(FenStruct.parse)
  }
}
