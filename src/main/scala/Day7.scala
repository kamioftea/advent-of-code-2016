import scala.io.Source

object Day7 {

  type NodeRef = String

  case class Node(name: String,
                  weight: Option[Int] = None,
                  parent: Option[NodeRef] = None,
                  children: Set[NodeRef] = Set.empty)

  private val LineMatcher = "([a-z]+) \\((\\d+)\\)(?: -> ((?:[a-z]+(?:, )?)+))?".r

  private def parseChildList(children: String): Set[NodeRef] =
    if (children == null) Set.empty
    else children.split(", ").toSet

  def parseInput(lines: TraversableOnce[String]): Map[NodeRef, Node] =
    lines.foldLeft(Map.empty[String, Node]) {
      case (map, LineMatcher(name, weight, children)) =>
        val node =
          map.getOrElse(name, Node(name))
            .copy(weight = Some(weight.toInt), children = parseChildList(children))

        node.children
          .foldLeft(map.updated(node.name, node)) {
            case (newMap, nodeRef) =>
              newMap.updated(
                nodeRef,
                newMap.getOrElse(nodeRef, Node(nodeRef))
                  .copy(parent = Some(node.name))
              )
          }

      case (map, _) => map
    }

  def findRoot(nodes: TraversableOnce[Node]): Option[Node] = {
    nodes.find(n => n.parent.isEmpty)
  }

  def findMismatchedNode(nodes: Map[NodeRef, Node]): Option[(Node, Int)] = {
    def iter(node: Node): (Int, Option[(Node, Int)]) = {
      val childData = node.children.toList.map(c_ref => (c_ref, iter(nodes(c_ref))))
      val totalWeight = node.weight.getOrElse(0) + childData.map { case (_, (w, _)) => w }.sum
      val groups = childData.groupBy { case (_, (w, _)) => w }
      (
        totalWeight,
        groups
          .find { case (_, ns) => ns.size == 1 }
          .map { case (cW, (c_ref, (_, maybeNode)) :: _) => maybeNode.getOrElse({
            val childNode = nodes(c_ref)
            (
              childNode,
              groups
              .find { case (_, ns) => ns.size > 1 }
              .map { case (w, _) => childNode.weight.getOrElse(0) - cW + w }
              .getOrElse(0)
            )
          })}
      )
    }

    val root = findRoot(nodes.values)
    root.flatMap { r => iter(r) match {
      case (_, maybeMismatch) => maybeMismatch
    }}

  }

  def main(args: Array[String]): Unit = {
    println(
      findRoot(
        parseInput(Source.fromResource("day7input.txt").getLines()).values
      )
    )
    println(
      findMismatchedNode(
        parseInput(Source.fromResource("day7input.txt").getLines())
      )
    )
  }
}

