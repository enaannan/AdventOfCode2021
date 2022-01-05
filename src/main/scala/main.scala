import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object main extends App {

  //Test Input
    val rawInput ="""7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
                    |
                    |22 13 17 11  0
                    | 8  2 23  4 24
                    |21  9 14 16  7
                    | 6 10  3 18  5
                    | 1 12 20 15 19
                    |
                    | 3 15  0  2 22
                    | 9 18 13 17  5
                    |19  8  7 25 23
                    |20 11 10 24  4
                    |14 21 16 12  6
                    |
                    |14 21 17 24  4
                    |10 16 15  9 19
                    |18  8 23 26 20
                    |22 11 13  6  5
                    | 2  0 12  3  7""".stripMargin.split("\\n")

  val input = rawInput.tail.tail
  val randomNumbers = rawInput(0).trim.split(",").map(_.toInt).toSeq
  var winningRandomNumber = 0
  val checkers: ArrayBuffer[ArrayBuffer[ArrayBuffer[Boolean]]] = ArrayBuffer()

  def initializeCheckers(length: Int): Unit = {

    def initialCheckerMatrix: ArrayBuffer[ArrayBuffer[Boolean]] = ArrayBuffer(
      ArrayBuffer(false, false, false, false, false),
      ArrayBuffer(false, false, false, false, false),
      ArrayBuffer(false, false, false, false, false),
      ArrayBuffer(false, false, false, false, false),
      ArrayBuffer(false, false, false, false, false))

    for (_ <- 0 until length) {
      checkers.addOne(initialCheckerMatrix)
    }
  }

  val boards = (input.flatMap { chars =>
    chars.split(" ")
  }.map(_.trim) filterNot (o => o.equals(""))).map(_.trim.toInt).grouped(5).toArray.grouped(5).toArray

  def getDim(s: Array[Array[Int]]): (Int, Int) = {
    val r = s.length
    val c = s(0).length
    (r, c)
  }

  def findPositions(arr: Array[Array[Array[Int]]], num: Int): Array[IndexedSeq[(Int, Int)]] = {

    def findPosition(arr: Array[Array[Int]], randomNumber: Int): IndexedSeq[(Int, Int)] = {
      val (r, c) = getDim(arr)
      for (i <- 0 until r; j <- 0 until c if arr(i)(j) == randomNumber) yield (i, j)
    }

    arr.map { mat => findPosition(mat, num) }
  }

  def markPositions(pos: Array[IndexedSeq[(Int, Int)]]): Unit = {
    for (k <- pos.indices) {
      if (pos(k).nonEmpty) {
        val (r, c) = pos(k).head
        checkers(k)(r).update(c, true)
      }
    }
  }

  def isWinner: Int = {

    @tailrec
    def findRowWinner(arr: ArrayBuffer[ArrayBuffer[Boolean]], start: Int): Boolean = {
      if (start < 5) {
        val winCount = arr(start).count(_ == true)
        if (winCount < 5) findRowWinner(arr, start + 1)
        else true
      }
      else false
    }

    val rowWinner: Int = checkers.map {
      board => findRowWinner(board, 0)
    }.indexOf(true)

    val columnWinner: Int = checkers.map {
      board => findRowWinner(board.transpose, 0)
    }.indexOf(true)

    val res: Int = if (rowWinner != -1) rowWinner
    else if (columnWinner != -1) columnWinner
    else -1
    res
  }

  @tailrec
  def findWinner(randomNumbers: Seq[Int], arr: Array[Array[Array[Int]]]): Int = {
    randomNumbers match {
      case Nil => 0

      case head +: tail =>

        val pos = findPositions(arr, head)
        markPositions(pos)
        if (isWinner == -1) findWinner(tail, arr)
        else {
          winningRandomNumber = head
          isWinner
        }

    }

  }

  def sumOfUnmarkedNumbers(board: Array[Array[Int]], checker: ArrayBuffer[ArrayBuffer[Boolean]]): Int = {

    @tailrec
    def sumSeq(seq: Seq[(Int, Int)], acc: Int): Int = {
      seq match {
        case head +: tail =>
          val (r, c) = head
          sumSeq(tail, (acc + board(r)(c)))

        case Nil => acc
      }
    }

    val (r, c) = getDim(board)
    val unmarkedPositions = for (i <- 0 until r; j <- 0 until c if !checker(i)(j)) yield (i, j)
    sumSeq(unmarkedPositions, 0)
  }

  initializeCheckers(boards.length)
  val winner = findWinner(randomNumbers, boards)

  println(s" board ${winner + 1} wins")

  val sum = sumOfUnmarkedNumbers(boards(winner), checkers(winner))

  println(sum * winningRandomNumber)
}