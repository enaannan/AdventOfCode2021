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

  val input = rawInput.map{line =>
    line.trim.toSeq.map{d =>
      d.toString.toInt
    }.toBuffer
  }

def validNeighbors(in: (Int, Int)):Seq[(Int,Int)] = {

  val right = if(in._2 + 1 <= colNumber) Seq((in._1,in._2+1)) else Seq() // adding right
  val left = if(in._2 - 1 > 0 ) Seq((in._1,in._2-1))else Seq()  // adding left
  val top = if(in._1 - 1 >0 ) Seq((in._1-1,in._2))else Seq()  // adding top
  val down = if(in._1 + 1 <= rowNumber) Seq((in._1,in._2+1)) else Seq() // adding down
 right ++ left ++ top ++ down
}

def step(in:Array[Buffer[Int]])={

    // steps though input till it meets a 9
    def simulate (in:Array[mutable.Buffer[Int]]): Unit ={
    for(i<- 0 to rowNumber;j<- 0 to colNumber){
      if(input(i)(j) > 8) flash(toVisit = Seq((i,j)))
    }
    }

    @tailrec
    def flash(toVisit:Seq[(Int,Int)]):Array[Buffer[Int]] ={
     toVisit match{
       case head +: tail =>
         if(input(head._1)(head._2) > 8) {
           input(head._1)(head._2) = 0 //reset(flash)
        val neighbours = validNeighbors(head)  //add neighbours of ref to tovisit
        flash(toVisit = tail ++ neighbours)
         } //set ref point to zero (flash)
         else {
           input(head._1)(head._2) +=1
         flash(tail)
         }
     }

    }

    val newInput: Array[mutable.Buffer[Int]] =  input.map{ row =>
      row.map{num=>
        num+1 }
    }

    simulate(newInput)
  }
  step(input)
  println(input)
