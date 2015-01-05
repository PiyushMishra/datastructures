package datastructures

object module extends App {

  val data = Array(2, 5, 6, -1, 8, 11, 12, 13, 14, 15, 16, 17,
    18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35)
  case class Node(var value: Option[Int] = None, var leftTree: Option[Node] = None, var rightTree: Option[Node] = None) {
    def isFull = this.leftTree.isDefined && this.rightTree.isDefined
    def isOnlyLeft = this.leftTree.isDefined && !this.rightTree.isDefined
    def isOnlyRight = !this.leftTree.isDefined && this.rightTree.isDefined
    def isInner = this.leftTree.isDefined || this.rightTree.isDefined
    def isLeaf = !this.leftTree.isDefined && !this.rightTree.isDefined
    override def toString = "{value is : " + value + "\nleft : " + leftTree + "\nright : " + rightTree + "}"
  }

  class BinaryTree(rootNodeValue: Option[Int]) {

    val rootNode = Node(rootNodeValue)

    def height = calculateHeight(Some(rootNode), 0)

    def balanced = isbalanced(Some(rootNode))

    def nodes = calculateNumberOfNodes(Some(rootNode), 0)

    override def toString = rootNode.toString

    def insert(value: Int, node: Node = rootNode): Unit = if (node.value.isDefined) {
      if (!node.leftTree.isDefined) {
        node.leftTree = Some(Node(Some(value)))
        println("node" + node.value + "says : " + node.leftTree.get.value + "is my left child")
      } else if (!node.rightTree.isDefined) {
        node.rightTree = Some(Node(Some(value)))
        println("node" + node.value + "says : " + node.rightTree.get.value + "is right my child")
      } else if (isbalanced(node.leftTree) && calculateNumberOfNodes(node.leftTree, 0) == calculateNumberOfNodes(node.rightTree, 0))
        insert(value, node.leftTree.get)
      else if (calculateNumberOfNodes(node.leftTree, 0) < Math.pow(2, calculateHeight(node.leftTree, 0)) - 1)
        insert(value, node.leftTree.get) else insert(value, node.rightTree.get)
    }
    def isbalanced(tree: Option[Node]) = {
      if (!tree.isDefined) false
      else {
        calculateNumberOfNodes(tree, 0) == Math.pow(2, calculateHeight(tree, 0)) - 1
      }
    }

    def calculateNumberOfNodes(tree: Option[Node], acc: Int): Int = {
      if (!tree.isDefined) acc else
        acc + 1 + calculateNumberOfNodes(tree.get.leftTree, 0) + calculateNumberOfNodes(tree.get.rightTree, 0)
    }

    def calculateHeight(tree: Option[Node], acc: Int): Int = {
      if (!tree.isDefined) acc
      else if (tree.get.isLeaf) acc + 1
      else List(calculateHeight(tree.get.leftTree, acc + 1), calculateHeight(tree.get.rightTree, acc + 1)).max
    }

  }

  val tree = new BinaryTree(Some(1))
  data.foreach { i => tree.insert(i) }
  println(tree.height)
  println(tree.nodes)
  println(tree.balanced)
}