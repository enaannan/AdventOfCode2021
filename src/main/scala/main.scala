import scala.annotation.tailrec

object main extends App {

  import scala.annotation.tailrec
  import scala.collection.mutable
  import scala.collection.mutable._

  val rawInput = """5483143223
                   |2745854711
                   |5264556173
                   |6141336146
                   |6357385478
                   |4167524645
                   |2176841721
                   |6882881134
                   |4846848554
                   |5283751526""".stripMargin.split("\\n")

  val colNumber = rawInput.head.trim.length
  val rowNumber = rawInput.length

  val input: Array[mutable.Buffer[Int]] = rawInput.map{ line =>
    line.trim.toSeq.map{d =>
      d.toString.toInt
    }.toBuffer
  }
  def increaseNeighbors(in:List[(Int,Int)])= {
  in.map{
    case (x, y) => input(x)(y) +=1
  }
  }

  def validNeighbors(in: (Int, Int)):List[(Int,Int)] = {

    val right = if(in._2 + 1 < colNumber) List((in._1,in._2+1)) else Nil // adding right
    val left = if(in._2 - 1 > 0 ) List((in._1,in._2-1))else Nil  // adding left
    val top = if(in._1 - 1 >0 ) List((in._1-1,in._2))else Nil  // adding top
    val down = if(in._1 + 1 < rowNumber) List((in._1+1,in._2)) else Nil // adding down

    right ++ left ++ top ++ down
  }

  @tailrec
  def flash(toVisit:List[(Int,Int)] ):Array[mutable.Buffer[Int]] ={
    toVisit match {
      case head +: tail =>
        if(input(head._1)(head._2) > 9) {
          input(head._1)(head._2) = 0 //reset(flash)
          val neighbours = validNeighbors(head)  //add neighbours of ref to tovisit
          increaseNeighbors(neighbours)
          flash(toVisit = tail ++ neighbours)
        } //set ref point to zero (flash)
        else {
          input(head._1)(head._2) +=1
          flash(tail)
        }
      case Nil => input
    }

  }

  // steps though input till it meets a 9
  def simulate : Unit ={
    for(i<- 0 until rowNumber;j<- 0 until colNumber){
      if(input(i)(j) > 9) flash(toVisit = List((i,j)))
    }
  }

  def step(steps:Int)={ // increases all ints by 1
   for(s <- 0 until steps ) {
    for(i<-0 until rowNumber;j<-0 until colNumber){
      input(i)(j) = input(i)(j)+1
    }
     simulate
   }

  }
  step(2)


  println(input)

}