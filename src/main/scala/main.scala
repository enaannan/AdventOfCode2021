import scala.annotation.tailrec
import scala.collection.mutable._

object main extends App {
//
//  val rawInput = """5483143223
//                 |2745854711
//                 |5264556173
//                 |6141336146
//                 |6357385478
//                 |4167524645
//                 |2176841721
//                 |6882881134
//                 |4846848554
//                 |5283751526""".stripMargin.split("\\n")
//
//val colNumber = rawInput.head.trim.length
//val rowNumber = rawInput.length
//
//val input = rawInput.map{line =>
//  line.trim.toSeq.map{d =>
//    d.toString.toInt
//  }.toBuffer
//}
//
//
//  def step(in:Array[Buffer[Int]])={
//
//    @tailrec
//    def flash(toVisit:Seq[(Int,Int)],visited:Seq[(Int,Int)]):Array[Buffer[Int]] ={
//
//    }
//
//  val newInput =  input.map{ row =>
//      row.map{num=>
//        num+1 }
//    }
//    newInput
//    flash(newInput)
//  }
//  step(input)
// println(input)
def sim1={
  def hi = true
  println("printing from sim1 before sim method call")
  sim(hi)
  println("printing from sim1 after sim method call")
}
  def sim2={
    lazy val hi = true
    println("printing from sim2 before sim method call")
    sim(hi)
    println("printing from sim2 after sim method call")
  }
  def sim(in: =>Boolean){
    if(true || in){
      println("we did run")
    }
  }
sim1

}