import Day7.Node
import org.scalatest.{FunSuite, Matchers}

class Day7Test extends FunSuite with Matchers {

  private def sample =
    """pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)""".stripMargin.lines

  private def subSample1 =
    """xhth (57)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |cntj (57)""".stripMargin.lines

  private def subSample2 =
    """pbga (67)
      |havc (66)
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq""".stripMargin.lines

  private def subSample3 =
    """ugmlq (68) -> gyxo, ebii
      |ebii (61)
      |gyxo (61)""".stripMargin.lines


  test("testParseInput") {

    val parsedSubSample1: Map[String, Node] = Day7.parseInput(subSample1)

    parsedSubSample1.size shouldBe 4
    parsedSubSample1.get("xhth") shouldBe Some(Node("xhth", Some(57), Some("fwft")))
    parsedSubSample1.get("ktlj") shouldBe Some(Node("ktlj", Some(57), Some("fwft")))
    parsedSubSample1.get("fwft") shouldBe Some(Node("fwft", Some(72), None, Set("xhth", "ktlj", "cntj")))
    parsedSubSample1.get("cntj") shouldBe Some(Node("cntj", Some(57), Some("fwft")))

    val parsedSubSample2: Map[String, Node] = Day7.parseInput(subSample2)

    parsedSubSample2.size shouldBe 4
    parsedSubSample2.get("pbga") shouldBe Some(Node("pbga", Some(67), Some("padx")))
    parsedSubSample2.get("havc") shouldBe Some(Node("havc", Some(66), Some("padx")))
    parsedSubSample2.get("padx") shouldBe Some(Node("padx", Some(45), None, Set("pbga", "havc", "qoyq")))
    parsedSubSample2.get("qoyq") shouldBe Some(Node("qoyq", Some(66), Some("padx")))

    val parsedSubSample3: Map[String, Node] = Day7.parseInput(subSample3)

    parsedSubSample3.size shouldBe 3
    parsedSubSample3.get("ebii") shouldBe Some(Node("ebii", Some(61), Some("ugmlq")))
    parsedSubSample3.get("gyxo") shouldBe Some(Node("gyxo", Some(61), Some("ugmlq")))
    parsedSubSample3.get("ugmlq") shouldBe Some(Node("ugmlq", Some(68), None, Set("ebii", "gyxo")))

    val parsedSample: Map[String, Node] = Day7.parseInput(sample)

    parsedSample.size shouldBe 13
    parsedSample.get("xhth") shouldBe Some(Node("xhth", Some(57), Some("fwft")))
    parsedSample.get("ktlj") shouldBe Some(Node("ktlj", Some(57), Some("fwft")))
    parsedSample.get("fwft") shouldBe Some(Node("fwft", Some(72), Some("tknk"), Set("xhth", "ktlj", "cntj")))
    parsedSample.get("cntj") shouldBe Some(Node("cntj", Some(57), Some("fwft")))
    parsedSample.get("pbga") shouldBe Some(Node("pbga", Some(66), Some("padx")))
    parsedSample.get("havc") shouldBe Some(Node("havc", Some(66), Some("padx")))
    parsedSample.get("padx") shouldBe Some(Node("padx", Some(45), Some("tknk"), Set("pbga", "havc", "qoyq")))
    parsedSample.get("qoyq") shouldBe Some(Node("qoyq", Some(66), Some("padx")))
    parsedSample.get("ebii") shouldBe Some(Node("ebii", Some(61), Some("ugml")))
    parsedSample.get("gyxo") shouldBe Some(Node("gyxo", Some(61), Some("ugml")))
    parsedSample.get("ugml") shouldBe Some(Node("ugml", Some(68), Some("tknk"), Set("ebii", "gyxo", "jptl")))
    parsedSample.get("jptl") shouldBe Some(Node("jptl", Some(61), Some("ugml")))
    parsedSample.get("tknk") shouldBe Some(Node("tknk", Some(41), None, Set("ugml", "padx", "fwft")))
  }

  test("testFindRoot") {
    Day7.findRoot(Day7.parseInput(subSample1).map { case (_, v) => v }).map(n => n.name) shouldBe Some("fwft")
    Day7.findRoot(Day7.parseInput(subSample2).values).map(n => n.name) shouldBe Some("padx")
    Day7.findRoot(Day7.parseInput(subSample3).values).map(n => n.name) shouldBe Some("ugmlq")
    Day7.findRoot(Day7.parseInput(sample).values).map(n => n.name) shouldBe Some("tknk")
  }

  test("testFindMismatchedWeight") {
    Day7.findMismatchedNode(Day7.parseInput(subSample2)) shouldBe
    Some((Node("pbga", Some(67), Some("padx")), 66))

    Day7.findMismatchedNode(Day7.parseInput(sample)) shouldBe
    Some((Node("ugml", Some(68), Some("tknk"), Set("ebii", "gyxo", "jptl")), 60))
  }

}
