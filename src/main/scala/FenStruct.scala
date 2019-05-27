case class FenStruct(
                      fields: Array[Array[Char]],
                      whoMove: Array[Char],
                      castling: Array[Char],
                      brokenField: Array[Char],
                      moves: Array[Int], // index 0 - ply, 1 - move
                      nextMove: Array[Char]
                    ) {

  def printAllFields(toExport: Boolean) = {
    var fieldsToString: String = ""
    if (toExport) fieldsToString = printFieldsAtExport("/")
    else fieldsToString = printFields("/")
    //    var strBrokenField = brokenField.mkString
    //    if (brokenField(0) == '-') strBrokenField = "-"
    var strBrokenField = "-"
    if (brokenField(0) != '-') strBrokenField = brokenField.mkString
    if (nextMove != null)
      println(fieldsToString + s" ${whoMove.mkString} ${castling.mkString} " +
        s"$strBrokenField ${moves(0)} ${moves(1)} ${nextMove.mkString}")
    else println(printFields("/") + s" ${whoMove.mkString} ${castling.mkString} " +
      s"$strBrokenField ${moves(0)} ${moves(1)}")
  }

  def printFields(separator: String): String = {
    var sum: String = ""
    fields match {
      case null => sum = "Wrong fields"
      case _ => sum = fields.indices.reverse.map(fields(_).mkString).foldLeft("")(_ + separator + _)
    }
    sum.subSequence(0, sum.length - 1).toString
  }

  def mkStringWithDots(arr: Array[Char]): String = {
    var newStr: String = ""
    var contin = 0
    for (elem <- arr.indices) {
      if (arr(elem) != '.') {
        if (contin > 0) {
          newStr += contin.toString
          contin = 0
        }
        newStr += arr(elem)
      }
      else {
        contin += 1
      }
    }
    if (contin > 0) newStr += contin.toString
    newStr
  }

  def printFieldsAtExport(separator: String): String = {
    fields match {
      case null => "Wrong fields"
      case _ => fields.indices.reverse.map(fields(_)).map(mkStringWithDots).foldLeft("")(_ + separator + _)
    }
  }

  def printFieldsAt(index: Int): String = {
    var sum: String = ""
    fields match {
      case null => sum = "Wrong fields"
      case _ => {
        for (elem <- fields(index - 1)) {
          if (elem.toString.matches("\\d")) {
            val num = elem.toString.toInt
            for (n <- 1 to num) sum = sum + " ."
          }
          else sum = sum + " " + elem
        }
      }
    }
    sum
  }

  def drawBoard = {
    val borderLine = "  +" + "-" * 17 + "+"
    println("\n" + borderLine)
    for (i <- (1 to 8).reverse) {
      println(i + " |" + printFieldsAt(i) + " |")
    }
    print(borderLine + "\n    ")
    for (i <- 'a' to 'h') print(i + " ")
    println("")
  }

  //сделаем ход
  def makeMove: FenStruct = {

    //если нет указания как ходить
    if (nextMove.sameElements(Array(0, 0, 0, 0, 0))) {
      println("Пустой ход")
      return this
    } else println(nextMove.mkString)

    //счетчик полного хода
    whoMove(0) = whoMove(0) match {
      case 'b' => {
        moves(1) += 1
        'w'
      }
      case 'w' => 'b'
      case _ => 'b'
    }

    //двигаем фигуру
    val yAxisCurrentPos = nextMove(1).toString.toInt
    val xAxisCurrentPos: Int = convertCharacterToNum(nextMove(0))
    //    println("Current position: x =" + xAxisCurrentPos + " y =" + yAxisCurrentPos)
    //фигура которая двигается
    val piece = fields(yAxisCurrentPos - 1)(xAxisCurrentPos - 1)
    fields(yAxisCurrentPos - 1).update(xAxisCurrentPos - 1, '.')

    val xAxisNextPos: Int = convertCharacterToNum(nextMove(2))
    val yAxisNextPos = nextMove(3).toString.toInt
    //если ход со взятием то фигура которую съедаем:
    val nextPiece = fields(yAxisNextPos - 1)(xAxisNextPos - 1)
    if (nextMove(4) != 0) { //превращение пешки
      fields(yAxisNextPos - 1).update(xAxisNextPos - 1, nextMove(4))
    } else fields(yAxisNextPos - 1).update(xAxisNextPos - 1, piece)

    //битое поле для белых
    if (piece == 'P') {
      val r = (yAxisCurrentPos + yAxisNextPos) / 2
      if (r == 3) {
        brokenField.update(0, convertNumToCharacter(xAxisCurrentPos))
        brokenField.update(1, '3')
      }
    }
    //битое поле для черных
    if (piece == 'p') {
      val r = (yAxisCurrentPos + yAxisNextPos) / 2
      if (r == 6) {
        brokenField.update(0, convertNumToCharacter(xAxisCurrentPos))
        brokenField.update(1, '6')
      }
    }

    //счетчик полуходов
    if (nextPiece != '.') { //если ход со взятием
      moves(0) = 0
    } else if (!(piece == 'p' || piece == 'P')) moves(0) += 1 //если ход пешкой

    //очищаем "следующий ход"
    for (i <- nextMove.indices) nextMove.update(i, 0)

    this
  }

  private def convertCharacterToNum(char: Char) = char.toInt - 96

  private def convertNumToCharacter(num: Int) = (num + 96).toChar

}

object FenStruct {
  def parse(s: String): Option[FenStruct] = {
    val patternWithoutMove = "^(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)".r
    val patternWithMove = "^(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)".r
    s match {
      case patternWithoutMove(fields, whoMove, castling, brokenField, halfMoves, moves) => {
        if (FenStruct.subparse(fields) == null) None
        else {
          val brokenFieldArr: Array[Char] = Array(0, 0)
          val tmpBfArr: Array[Char] = brokenField.toCharArray
          for (i <- tmpBfArr.indices) brokenFieldArr.update(i, tmpBfArr(i))

          Some(FenStruct(FenStruct.subparse(fields), whoMove.toCharArray, castling.toCharArray, brokenFieldArr,
            Array(halfMoves.toInt, moves.toInt), Array(0, 0, 0, 0, 0)))
        }

        //          Array(halfMoves.toInt, moves.toInt), Array(0, 0, 0, 0)))
      }
      case patternWithMove(fields, whoMove, castling, brokenField, halfMoves, moves, nextMove) => {
        val nextMoveArr: Array[Char] = Array(0, 0, 0, 0, 0)
        val tmpArr: Array[Char] = nextMove.toCharArray
        for (i <- tmpArr.indices) nextMoveArr.update(i, tmpArr(i))

        val brokenFieldArr: Array[Char] = Array(0, 0)
        val tmpBfArr: Array[Char] = brokenField.toCharArray
        for (i <- tmpBfArr.indices) brokenFieldArr.update(i, tmpBfArr(i))

        Some(FenStruct(FenStruct.subparse(fields), whoMove.toCharArray, castling.toCharArray, brokenFieldArr,
          //          Array(halfMoves.toInt, moves.toInt), nextMove.toCharArray))
          Array(halfMoves.toInt, moves.toInt), nextMoveArr))
      }
      case _ => None
    }
  }

  def subparse(line: String): Array[Array[Char]] = {
    val pattern = "^(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)".r
    line match {
      case pattern(line8, line7, line6, line5, line4, line3, line2, line1) => {
        Array(convertDigit(line1).toCharArray, convertDigit(line2).toCharArray, convertDigit(line3).toCharArray,
          convertDigit(line4).toCharArray, convertDigit(line5).toCharArray, convertDigit(line6).toCharArray,
          convertDigit(line7).toCharArray, convertDigit(line8).toCharArray)
      }
      case _ => null
    }
  }

  def convertDigit(str: String) = {
    var outStr: String = ""
    for (i <- str) {
      if (i >= '0' && i <= '9') {
        outStr = outStr + "." * i.toString.toInt
      }
      else outStr = outStr + i
    }
    outStr
  }


}