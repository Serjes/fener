import scala.io.{Source, StdIn}

object Main {
  val inputFileWithFens = "fens_with_moves.txt"
  val inputFileWithFensAnswers = "fens_with_moves_answers.txt"

  def main(args: Array[String]): Unit = {
    val listOfFens: List[FenStruct] = getInputFens(inputFileWithFens)
    val listOfAnswers: List[FenStruct] = getInputFens(inputFileWithFensAnswers)

    println("Дано: ")
    listOfFens.foreach(_.printAllFields(true))
    println("Надо: ")
    listOfAnswers.foreach(_.printAllFields(true))

    println("Обработаем ходы(берем fen-ы только с ходами):")
    val doubleList = listOfFens.filter(_.nextMove != null).zip(listOfAnswers)
    val counter = 0
    makeMovesForListOfFens(doubleList, counter)

    //            listOfFens.foreach(_.drawBoard)
    //    listOfFens(2).printAllFields
    //    listOfFens(2).drawBoard
    //    listOfFens(2).makeMove.printAllFields
//    processingOneFen()
  }

  private def processingOneFen(): Unit = {
    println("Введите строку FEN:")
    val fen = StdIn.readLine()
    println("Введите ход:")
    val parsedFen = FenStruct.parse(fen + " " + StdIn.readLine())
    if (parsedFen.isDefined) {
      parsedFen.get.drawBoard
      val processedFen = parsedFen.get.makeMove
      processedFen.printAllFields(true)
      processedFen.drawBoard

    }
    else println("Некорректный FEN")

  }

  private def makeMovesForListOfFens(fensAndAnswers: List[(FenStruct, FenStruct)], counter: Int): Unit = {
    val counter1 = counter + 1
    fensAndAnswers match {
      case head :: tail => {
        println(s"\n$counter1) Выполняем ход:")
        println("Нарисуем перед ходом")
        head._1.drawBoard
        head._1.makeMove.printAllFields(true)
        println("Сравните с правильным ответом:")
        head._2.printAllFields(true)
        if (head._1.whoMove.sameElements(head._2.whoMove)
          && head._1.castling.sameElements(head._2.castling)
          && head._1.brokenField.sameElements(head._2.brokenField)
          && head._1.moves.sameElements(head._2.moves)
          //          && head._1.nextMove.sameElements(head._2.nextMove)
          && eq(head._1.fields, head._2.fields)
        ) println("Совпадают")
        else println("Несовпадают!!!")
        head._1.drawBoard
        makeMovesForListOfFens(tail, counter1)
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
