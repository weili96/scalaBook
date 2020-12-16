package no23

object Queen {


  def queens(n:Int):List[List[(Int,Int)]] = {

    def placeQueens(k:Int):List[List[(Int,Int)]] = {
      if (k == 0)
        List(List())
      else
        for{
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k,column)
          if isSafe(queen,queens)
        } yield queen :: queens
    }

    def isSafe(queen: (Int, Int), queens: List[(Int, Int)]):Boolean = {
      queens forall (q => !inCheck(queen,q))
    }

    def inCheck(q1: (Int, Int), q2: (Int, Int)):Boolean = {
      q1._1 == q2._1 ||
      q1._2 == q2._2 ||
      (q1._1 - q2._1).abs == (q1._2 - q2._2)
    }

    placeQueens(n)

  }


  def main(args: Array[String]): Unit = {

    val result = queens(8)

    result.foreach(println)

  }

}
