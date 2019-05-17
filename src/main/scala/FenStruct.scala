case class FenStruct(
                      fields: Array[Char],
                      whoMove: Array[Char],
                      castling: Array[Char],
                      brokenField: Array[Char],
                      halfMoves: Array[Char],
                      moves: Array[Char]
                    ) {
  def printAll: Unit = println(s"${fields.toStream.slice(0, 8).mkString} and  ${whoMove.mkString} and ${castling.mkString} and ${brokenField.mkString}")

}

object FenStruct {
  def parse(s: String) = {
    val pattern = "^(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)\\s(\\S+)".r
    val struct: Option[FenStruct] = s match {
      case pattern(fields, whoMove, castling, brokenField, halfMoves, moves) =>
        Some(FenStruct(fields.toCharArray, whoMove.toCharArray, castling.toCharArray, brokenField.toCharArray,
          halfMoves.toCharArray, moves.toCharArray))
//        for {_ <- fields
//             _ <- whoMove
//             _ <- castling
//             _ <- brokenField
//             _ <- halfMoves
//             _ <- moves
//        } yield FenStruct(fields, whoMove, castling, brokenField, halfMoves, moves)
      case _ => None
    }
    if (struct.isDefined)struct.get.printAll
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