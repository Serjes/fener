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
    if (nextMove != null)
      println(fieldsToString + s" ${whoMove.mkString} ${castling.mkString} " +
        s"${brokenField.mkString} ${moves(0)} ${moves(1)} ${nextMove.mkString}")
    else println(printFields("/") + s" ${whoMove.mkString} ${castling.mkString} " +
      s"${brokenField.mkString} ${moves(0)} ${moves(1)}")
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
        if (contin > 0 ) {
          newStr += contin.toString
          contin = 0
        }
        newStr += arr(elem)
      }
      else {
        contin += 1
      }
    }
    if (contin > 0 ) newStr += contin.toString
    newStr
  }

  def printFieldsAtExport(separator: String): String = {
    var sum: String = ""
    fields match {
      case null => sum = "Wrong fields"
      case _ => sum = fields.indices.reverse.map(fields(_)).map(mkStringWithDots).foldLeft("")(_ + separator + _)
    }
    sum.subSequence(0, sum.length - 1).toString
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

  def makeMove: FenStruct = {

    if (nextMove.sameElements(Array(0,0,0,0))) {
      println("Пустой ход")
      return this
    }
    whoMove(0) = whoMove(0) match {
      case 'b' => {
        moves(1) += 1
        'w'
      }
      case 'w' => 'b'
      case _ => 'b'
    }
    val letter = nextMove(0)
    val yAxisCurrentPos = nextMove(1).toString.toInt

    val xAxisCurrentPos: Int = letter match {
      case 'a' => 1
      case 'b' => 2
      case 'c' => 3
      case 'd' => 4
      case 'e' => 5
      case 'f' => 6
      case 'g' => 7
      case 'h' => 8
    }
    println("Current position: x =" + xAxisCurrentPos + " y =" + yAxisCurrentPos)
    println(fields(yAxisCurrentPos - 1).mkString)
//    fields(yAxisCurrentPos).update(xAxisCurrentPos, '.')

    moves(0) += 1 // полуходы

    for (i <- nextMove.indices) nextMove.update(i, 0) //очищаем "следующий ход"

    this
  }


}

object FenStruct {
  def parse(s: String): Option[FenStruct] = {
    val pattern = "^(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)".r
    val patternWithMove = "^(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)".r
    s match {
      case pattern(fields, whoMove, castling, brokenField, halfMoves, moves) => {
        if (FenStruct.subparse(fields) == null) None
        else Some(FenStruct(FenStruct.subparse(fields), whoMove.toCharArray, castling.toCharArray, brokenField.toCharArray,
          Array(halfMoves.toInt, moves.toInt), Array(0, 0, 0, 0)))
      }
      case patternWithMove(fields, whoMove, castling, brokenField, halfMoves, moves, nextMove) =>
        Some(FenStruct(FenStruct.subparse(fields), whoMove.toCharArray, castling.toCharArray, brokenField.toCharArray,
          Array(halfMoves.toInt, moves.toInt), nextMove.toCharArray))
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
    for(i <- str) {
      if (i >= '0' && i <= '9') {
        outStr = outStr + "." * i.toString.toInt
      }
      else outStr = outStr + i
    }
    outStr
  }


}