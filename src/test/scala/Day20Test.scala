import Day20._
import org.scalatest.{FunSuite, Matchers}

class Day20Test extends FunSuite with Matchers {

  test("coordinates can be ordered") {
    Seq(
      Coordinate(0, -1, 0),
      Coordinate(0, -1, 3),
      Coordinate(2, 0, 0),
      Coordinate(0, 0, 0),
      Coordinate(1, -1, 1)
    ).sorted shouldBe Seq(
      Coordinate(0, 0, 0),
      Coordinate(0, -1, 0),
      Coordinate(2, 0, 0),
      Coordinate(1, -1, 1),
      Coordinate(0, -1, 3)
    )
  }

  test("can parse lines") {
    parseLines(
      """p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
        |p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>""".stripMargin.lines
    ) shouldBe Seq(
      Particle(
        0,
        Coordinate(3, 0, 0),
        Coordinate(2, 0, 0),
        Coordinate(-1, 0, 0)
      ),
      Particle(
        1,
        Coordinate(4, 0, 0),
        Coordinate(0, 0, 0),
        Coordinate(-2, 0, 0)
      )
    )
  }

  test("can step particles") {
    Particle(
      0,
      Coordinate(3, 0, 0),
      Coordinate(2, 0, 0),
      Coordinate(-1, 0, 0)
    ).next shouldBe
      Particle(
        0,
        Coordinate(4, 0, 0),
        Coordinate(1, 0, 0),
        Coordinate(-1, 0, 0)
      )

    Particle(
      0,
      Coordinate(3, 0, 0),
      Coordinate(2, 0, 0),
      Coordinate(-1, 0, 0)
    ).next.next shouldBe
      Particle(
        0,
        Coordinate(4, 0, 0),
        Coordinate(0, 0, 0),
        Coordinate(-1, 0, 0)
      )


    Particle(
      1,
      Coordinate(4, 0, 0),
      Coordinate(0, 0, 0),
      Coordinate(-2, 0, 0)
    ).next shouldBe
      Particle(
        1,
        Coordinate(2, 0, 0),
        Coordinate(-2, 0, 0),
        Coordinate(-2, 0, 0)
      )
  }

  test("can collide particles") {
    countSurvivingParticles(
      parseLines(
        """p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
          |p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
          |p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
          |p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>""".stripMargin.lines
      )
    ) shouldBe 1
  }

}
