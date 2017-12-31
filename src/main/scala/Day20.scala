import scala.collection.immutable
import scala.io.Source

object Day20 {

  case class Coordinate(x: Long, y: Long, z: Long) {
    def +(that: Coordinate): Coordinate = copy(x = x + that.x, y = y + that.y, z = z + that.z)

    lazy val magnitude: Long = Math.abs(x) + Math.abs(y) + Math.abs(z)
  }

  case class Particle(index: Long, pos: Coordinate, vel: Coordinate, acc: Coordinate) {
    def next: Particle = copy(pos = pos + vel + acc, vel = vel + acc)
  }

  implicit val coordinateOrder: Ordering[Coordinate] = Ordering.by(_.magnitude)

  private val LineMatcher = "p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, a=<(-?\\d+),(-?\\d+),(-?\\d+)>".r

  def parseLines(lines: TraversableOnce[String]): Seq[Particle] =
    lines.toSeq.zipWithIndex.collect {
      case (LineMatcher(px, py, pz, vx, vy, vz, ax, ay, az), i) => Particle(
        i,
        Coordinate(px.toLong, py.toLong, pz.toLong),
        Coordinate(vx.toLong, vy.toLong, vz.toLong),
        Coordinate(ax.toLong, ay.toLong, az.toLong)
      )
    }

  def countSurvivingParticles(particles: Seq[Particle], count: Long = 0, prevLength: Int = 0): Int = {
    val newParticles: Seq[Particle] =
      particles
        .groupBy(_.pos)
        .filter {case (_, ps) => ps.lengthCompare(1) == 0}
        .flatMap(_._2)
        .map(_.next)
        .toSeq

    if(count % 1000 == 0) {
      if(newParticles.lengthCompare(prevLength) == 0)
        newParticles.length
      else
        countSurvivingParticles(newParticles, count + 1, newParticles.length)
    }
    else countSurvivingParticles(newParticles, count + 1, prevLength)
  }

  def main(args: Array[String]): Unit = {
    val input = parseLines(Source.fromResource("day20input.txt").getLines())

    input.sortBy(_.acc).take(3).foreach(println)

    println(countSurvivingParticles(input))
  }
}
