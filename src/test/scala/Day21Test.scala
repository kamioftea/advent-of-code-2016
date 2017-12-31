import Day21._
import org.scalatest.{FunSuite, Matchers}

class Day21Test extends FunSuite with Matchers {

  test("can malipulate vectors")
  {
    Vector("#.".toVector, "##".toVector).flip shouldBe Vector(".#".toVector, "##".toVector)
    Vector("#.".toVector, "##".toVector).rot shouldBe Vector("##".toVector, "#.".toVector)
    Vector("#.".toVector, "##".toVector).rot.rot shouldBe Vector("##".toVector, ".#".toVector)
    Vector("#.".toVector, "##".toVector).rot.rot.rot shouldBe Vector(".#".toVector, "##".toVector)
    Vector("#.".toVector, "##".toVector).rot.rot.rot.rot shouldBe Vector("#.".toVector, "##".toVector)
  }

  test("can parse input") {
    parseInput(Seq(
      "##/#. => .../.##/##.",
      "##/## => ###/#../#.."
    )) shouldBe Map(
      Vector("##".toVector, "#.".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector(".#".toVector, "##".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("#.".toVector, "##".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("##".toVector, ".#".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("##".toVector, "##".toVector) -> Vector("###".toVector, "#..".toVector, "#..".toVector)
    )
    parseInput(Seq(
      ".../.../... => .#../.#../#..#/##..",
      "#../.../... => ####/####/.###/####"
    )) shouldBe Map(
      Vector("...".toVector, "...".toVector, "...".toVector) -> Vector(".#..".toVector, ".#..".toVector, "#..#".toVector, "##..".toVector),
      Vector("#..".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("..#".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "#..".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "..#".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector)
    )
    parseInput(Seq(
      ".../.../... => .#../.#../#..#/##..",
      "##./.../... => ####/####/.###/####"
    )) shouldBe Map(
      Vector("...".toVector, "...".toVector, "...".toVector) -> Vector(".#..".toVector, ".#..".toVector, "#..#".toVector, "##..".toVector),
      Vector("##.".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector(".##".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "##.".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, ".##".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("..#".toVector, "..#".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("#..".toVector, "#..".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "#..".toVector, "#..".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "..#".toVector, "..#".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector)
    )

    parseInput(Seq(
      "##/#. => .../.##/##.",
      "##/## => ###/#../#..",
      ".../.../... => .#../.#../#..#/##..",
      "#../.../... => ####/####/.###/####"
    )) shouldBe Map(
      Vector("##".toVector, "#.".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector(".#".toVector, "##".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("#.".toVector, "##".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("##".toVector, ".#".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("##".toVector, "##".toVector) -> Vector("###".toVector, "#..".toVector, "#..".toVector),
      Vector("...".toVector, "...".toVector, "...".toVector) -> Vector(".#..".toVector, ".#..".toVector, "#..#".toVector, "##..".toVector),
      Vector("#..".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("..#".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "#..".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "..#".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector)
    )
  }

  private val book = parseInput(Seq(
    "../.# => ##./#../...",
    ".#./..#/### => #..#/..../..../#..#"
  ))

  private val base = Vector(
    ".#.",
    "..#",
    "###"
  ).map(_.toVector)

  test("can expand image") {
    expand(base, book, 1) shouldBe Vector(
      "#..#",
      "....",
      "....",
      "#..#"
    ).map(_.toVector)

    expand(base, book, 2) shouldBe Vector(
      "##.##.",
      "#..#..",
      "......",
      "##.##.",
      "#..#..",
      "......"
    ).map(_.toVector)
  }


  test("can count pixels image") {
    countPixels(base, book, 2) shouldBe 12
  }
}
