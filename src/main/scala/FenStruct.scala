case class FenStruct(
                      fields: Array[Char],
                      whoMove: Array[Char],
                      castling: Array[Char],
                      brokenField: Array[Char],
                      halfMoves: Array[Char],
                      moves: Array[Char]
                    ) {
//  def printAll= println(s"${fields.toStream.slice(0, 8).mkString} and  ${whoMove.mkString} and ${castling.mkString} and ${brokenField.mkString}")
  def printAll= println(s"${fields.mkString} and  ${whoMove.mkString} and ${castling.mkString} and " +
    s"${brokenField.mkString} and ${halfMoves.mkString} and ${moves.mkString}")

  def draw = {

  }

}

object FenStruct {
  def parse(s: String): Option[FenStruct] = {
    val pattern = "^(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)".r
    s match {
      case pattern(fields, whoMove, castling, brokenField, halfMoves, moves) =>
        Some(FenStruct(fields.toCharArray, whoMove.toCharArray, castling.toCharArray, brokenField.toCharArray,
          halfMoves.toCharArray, moves.toCharArray))
      case _ => None
    }
  }

  def subparse(line: String) = {
    val pattern = "^(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)\\/(\\S+)".r
    val subLine = line match {
      case pattern(line1, line2, line3, line4, line5, line6) => None
//        for {
//          c1 <- line1
//          c2 <- line2
//        } yield Array(c1),Array(c2)


    }
  }
}