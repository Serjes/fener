case class FenStruct(
                      fields: Array[Array[Char]],
                      whoMove: Array[Char],
                      castling: Array[Char],
                      brokenField: Array[Char],
                      //                      halfMoves: Array[Char],
                      //                      moves: Array[Char],
                      //                      moves: Int,
                      moves: Array[Int], // 0 - halfMoves, 1 - move
                      nextMove: Array[Char]
                    ) {

  def printAllFields = {
    if (nextMove != null)
      println(printFields("/") + s" ${whoMove.mkString} ${castling.mkString} " +
        //      s"${brokenField.mkString} ${halfMoves.mkString} ${moves.mkString} ${nextMove.mkString}")
        s"${brokenField.mkString} ${moves(0)} ${moves(1)} ${nextMove.mkString}")
    else println(printFields("/") + s" ${whoMove.mkString} ${castling.mkString} " +
      s"${brokenField.mkString} ${moves(0)} ${moves(1)}")
  }

  def printFields(separator: String): String = {
    var sum: String = ""
    fields match {
      case null => sum = "Wrong fields"
      case _ => for (elem <- fields.reverse) sum += (elem.mkString + separator)
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
    println(borderLine)
  }

  def makeMove = {
    whoMove(0) = whoMove(0) match {
      case 'b' => 'w'
      case 'w' => 'b'
      case _ => 'b'
    }


    //    val movesArr: Array[Char] = (moves.mkString.toInt + 1).toString.toCharArray
    //    for (i <- moves.indices) moves.update(i,movesArr(i))
    moves(1) += 1

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
          //          halfMoves.toCharArray, moves.toCharArray, null))
          //          halfMoves.toCharArray, moves.toCharArray, Array(0,0,0,0)))
          Array(halfMoves.toInt, moves.toInt), Array(0, 0, 0, 0)))
      }
      case patternWithMove(fields, whoMove, castling, brokenField, halfMoves, moves, nextMove) =>
        Some(FenStruct(FenStruct.subparse(fields), whoMove.toCharArray, castling.toCharArray, brokenField.toCharArray,
          //          halfMoves.toCharArray, moves.toCharArray, nextMove.toCharArray))
          //          halfMoves.toCharArray, Array(moves.toInt), nextMove.toCharArray))
          Array(halfMoves.toInt, moves.toInt), nextMove.toCharArray))
      case _ => None
    }
  }

  def subparse(line: String): Array[Array[Char]] = {
    val pattern = "^(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)".r
    line match {
      case pattern(line8, line7, line6, line5, line4, line3, line2, line1) => {
        Array(line1.toCharArray, line2.toCharArray, line3.toCharArray, line4.toCharArray, line5.toCharArray,
          line6.toCharArray, line7.toCharArray, line8.toCharArray)
      }
      case _ => null
    }
  }


}