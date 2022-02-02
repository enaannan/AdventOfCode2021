import scala.annotation.tailrec
import scala.collection.immutable.ListMap

//val rawInput = Seq(3,4,3,1,2) //test input
val rawInput = Seq(2,5,3,4,4,5,3,2,3,3,2,2,4,2,5,4,1,1,4,4,5,1,2,1,5,2,1,5,1,1,1,2,4,3,3,1,4,2,3,4,5,1,2,5,1,2,2,5,2,4,4,1,4,5,4,2,1,5,5,3,2,1,3,2,1,4,2,5,5,5,2,3,3,5,1,1,5,3,4,2,1,4,4,5,4,5,3,1,4,5,1,5,3,5,4,4,4,1,4,2,2,2,5,4,3,1,4,4,3,4,2,1,1,5,3,3,2,5,3,1,2,2,4,1,4,1,5,1,1,2,5,2,2,5,2,4,4,3,4,1,3,3,5,4,5,4,5,5,5,5,5,4,4,5,3,4,3,3,1,1,5,2,4,5,5,1,5,2,4,5,4,2,4,4,4,2,2,2,2,2,3,5,3,1,1,2,1,1,5,1,4,3,4,2,5,3,4,4,3,5,5,5,4,1,3,4,4,2,2,1,4,1,2,1,2,1,5,5,3,4,1,3,2,1,4,5,1,5,5,1,2,3,4,2,1,4,1,4,2,3,3,2,4,1,4,1,4,4,1,5,3,1,5,2,1,1,2,3,3,2,4,1,2,1,5,1,1,2,1,2,1,2,4,5,3,5,5,1,3,4,1,1,3,3,2,2,4,3,1,1,2,4,1,1,1,5,4,2,4,3)


def initializeInput(in:Seq[Int]):ListMap[Int,Long]={
  val input: ListMap[Int,Long] =
    ListMap((-1,0),
      (0,in.count(_==0)),
      (1,in.count(_==1)),
      (2,in.count(_==2)),
      (3,in.count(_==3)),
      (4,in.count(_==4)),
      (5,in.count(_==5)),
      (6,in.count(_==6)),
      (7,in.count(_==7)),
      (8,in.count(_==0)))
  input
}




@tailrec
def tick(in:Map[Int,Long], iterator:Int=0):Map[Int,Long]={
  if(iterator<=8) {
    tick(
      in.updated(iterator-1,in(iterator)),
      iterator+1)
  }
  else {
    val rof = in.updated(6,in(-1)+in(6)) // rof: respawn old fish
    rof.updated(8,in(-1))
  }
}

@tailrec
def simulate(in:Map[Int,Long], days:Int):Map[Int,Long]={
  if(days>0)simulate(tick(in),days-1)
  else in
}


val p = simulate(initializeInput(rawInput),256)
p.values.sum - p(-1)
