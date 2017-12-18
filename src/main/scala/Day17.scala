object Day17 {
  def spin(iterations:Int,
           step: Int,
           pos: Int = 0,
           buffer: Vector[Int] = Vector(0),
           current: Int = 0)
  :(Int, Vector[Int]) =
    if(current == iterations) (pos, buffer)
    else {
      val splitPos = ((pos + step) % buffer.length) + 1
      val (pre, post) = buffer.splitAt(splitPos)

      spin(iterations,step,splitPos, pre ++ Vector(current + 1) ++ post, current + 1)
    }

  def getSubsequentValue(target: Int, step:Int): Int = {
    val (pos, buffer) = spin(target, step)

    buffer((pos + 1) % buffer.length)
  }

  def trackZero(iterations: Int,
                step: Int,
                currPos: Int = 0,
                zeroPos: Int = 0,
                valueAfterZero: Int = 0,
                current: Int = 0)
  : Int =
    if(current == iterations) valueAfterZero
    else {
      val newPos = if(current == 0) 1 else ((currPos + step) % (current + 1)) + 1
      trackZero(
        iterations,
        step,
        newPos,
        if(newPos <= zeroPos) zeroPos + 1 else zeroPos,
        if(newPos == zeroPos + 1) current + 1 else valueAfterZero,
        current + 1
      )
    }


  def main(args: Array[String]): Unit = {
    println(getSubsequentValue(2017, 337))
    println(trackZero(50000000, 337))
  }
}
