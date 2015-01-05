package datastructures

object module extends App {

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

    override def toString = rootNode.toString

    def insert(value: Int, node: Node = rootNode): Unit = if (node.value.isDefined) {

      if (!node.leftTree.isDefined) {
        node.leftTree = Some(Node(Some(value)))
        println("node" + node.value + "says : " + node.leftTree.get.value + "is my child")
      } else if (!node.rightTree.isDefined) {
        node.rightTree = Some(Node(Some(value)))
        println("node" + node.value + "says : " + node.rightTree.get.value + "is my child")
      } else if (node.leftTree.get.isFull && node.rightTree.get.isFull)
        insert(value, node.leftTree.get)
      else if (node.leftTree.get.isFull)
        insert(value, node.rightTree.get) else insert(value, node.leftTree.get)
    }
  }

  val data = Array(2, 3, 4, 5, 6, 7, 8, 9)

  val tree = new BinaryTree(Some(1))
  data.foreach { i => tree.insert(i) }
}