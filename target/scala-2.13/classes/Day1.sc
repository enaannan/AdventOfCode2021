import scala.collection.mutable.ListBuffer

val rawInput: Array[String] = Utils.readInput("Day1m")

val inputs:Array[Int]= for(line<-rawInput)yield Array(line.toInt)
//val inputs:Array[Int] = RawInputs.split("\\n").map(_.toInt)

def countIncreased(input: Array[Int]): Int = {
  var counter = 0
  for (n <- 0 until (input.length - 1)) {
    if (input(n) - input(n + 1) < 0) counter = counter + 1
  }
  counter
}

def getWindowSum(start: Int, input: Array[Int]): Int = {
  var sum = 0
  val window = for (n <- start to (start + 2)) yield input(n)
  window.foreach(sum += _)
  sum
}

val inputs2: ListBuffer[Int] = ListBuffer()
for (i <- 0 until (inputs.length - 2) by 2) {
  inputs2.addOne(getWindowSum(i, inputs))
  inputs2.addOne(getWindowSum(i + 1, inputs))
}

val count = countIncreased(inputs)
println(count)

val count2 = countIncreased(inputs2.toArray)
println(count2)
