object Day1_update {

val inputs: Array[Int] =  Utils.readInput("Inputs/Day1m").map(_.toInt)

  //part one
  def countIncrease(input:Array[Int] = inputs): Int = input.sliding(2).count{ arr => arr(0) < arr(1)  }

  //part two
def countIncreasePartTwo: Int =  countIncrease(inputs.sliding(3).map(_.sum).toArray)

}
