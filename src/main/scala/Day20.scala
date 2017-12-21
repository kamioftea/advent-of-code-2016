import scala.collection.immutable
import scala.io.Source

object Day20 {

  case class Coordinate(x: Long, y: Long, z: Long) {
    def +(that: Coordinate): Coordinate = copy(x = x + that.x, y = y + that.y, z = z + that.z)

    lazy val mag: Long = Math.abs(x) + Math.abs(y) + Math.abs(z)
  }

  case class Particle(index: Long, pos: Coordinate, vel: Coordinate, acc: Coordinate) {
    def next: Particle = copy(pos = pos + vel + acc, vel = vel + acc)
  }

  implicit val coordinateOrder: Ordering[Coordinate] = Ordering.by(_.mag)
  implicit val particleOrder: Ordering[Particle] = Ordering.by(_.pos)
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

  def main(args: Array[String]): Unit = {
    val input = parseLines(Source.fromResource("day20input.txt").getLines())

    def iter(particles: Seq[Particle], count: Long = 10000000): Unit = {
      if (count % 1000 == 0) println(particles.min)

      if (count != 0) iter(particles.map(_.next), count - 1)
    }

    val minAcc = input.minBy(_.acc)

    //iter(input.filter(p => p.acc.mag == minAcc.acc.mag))

    def iterCollisions(particles: Seq[Particle], count: Long = 10000000): Unit = {
      val newParticles: Seq[Particle] =
        particles
          .groupBy(_.pos)
          .filter {case (pos, ps) => ps.length == 1}
          .flatMap(_._2)
          .map(_.next)
          .toSeq

      if(count % 1000 == 0) println(newParticles.length)

      if (count != 0) iterCollisions(newParticles, count - 1)
    }

    iterCollisions(input)

  }
}
