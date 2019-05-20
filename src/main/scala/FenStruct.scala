case class FenStruct(
                      fields: Array[Array[Char]],
                      whoMove: Array[Char],
                      castling: Array[Char],
                      brokenField: Array[Char],
                      halfMoves: Array[Char],
                      moves: Array[Char]
                    ) {
  def printAll= println(printFields + s" and  ${whoMove.mkString} and ${castling.mkString} and " +
    s"${brokenField.mkString} and ${halfMoves.mkString} and ${moves.mkString}")
  def printFields: String = {
    var sum: String = ""
    for(elem <- fields.reverse) sum = sum + " " + elem.mkString
    sum
  }

  def printFieldsAt(index: Int): String = {
    var sum: String = ""
    for (elem <- fields(index - 1)) {
      if (elem.toString.matches("\\d")) {
        val num = elem.toString.toInt
        for (n <- 1 to num) sum = sum + " ."
      }
      else sum = sum + " " + elem
    }
    sum
  }

  def draw = {
    val borderLine = "  +" + "-"*17 + "+"
    println("\n" + borderLine)
    for (i <- (1 to 8).reverse) {
      println(i + " |" + printFieldsAt(i) + " |")
    }
    println(borderLine)
  }

}

object FenStruct {
  def parse(s: String): Option[FenStruct] = {
    val pattern = "^(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)".r
    s match {
      case pattern(fields, whoMove, castling, brokenField, halfMoves, moves) => {
        Some(FenStruct(FenStruct.subparse(fields), whoMove.toCharArray, castling.toCharArray, brokenField.toCharArray,
          halfMoves.toCharArray, moves.toCharArray))
      }
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
//      case _ => Array(Array(null))


    }
  }
}