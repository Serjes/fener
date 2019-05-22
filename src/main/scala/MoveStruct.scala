case class MoveStruct(moves: Array[Char]) {
  def printAll = {
    println(moves.mkString)
  }
}

object MoveStruct {
  def parse(s: String): Option[MoveStruct] = {
//    val pattern = "^([a-zA-Z]\\d[a-zA-Z]\\dd)".r
//    val pattern = "^(\\w\\w\\w\\w)".r
//    val pattern = "^(\\w\\d\\w\\d)".r
    val pattern = "^([a-zA-Z]\\d[a-zA-Z]\\d)".r
    s match {
      case pattern(move) => Some(MoveStruct(move.toCharArray))
      case _ => None
    }
  }
}
