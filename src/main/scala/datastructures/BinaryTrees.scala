package datastructures

object module extends App {

  val data = Array(26,25,19,17,1,90,36)
  case class Node(var value: Option[Int] = None, var leftTree: Option[Node] = None, var rightTree: Option[Node] = None) {
    def isFull = this.leftTree.isDefined && this.rightTree.isDefined
    def isOnlyLeft = this.leftTree.isDefined && !this.rightTree.isDefined
    def isOnlyRight = !this.leftTree.isDefined && this.rightTree.isDefined
    def isInner = this.leftTree.isDefined || this.rightTree.isDefined
    def isLeaf = !this.leftTree.isDefined && !this.rightTree.isDefined

    override def toString = if (isOnlyLeft) {
      "node" + this.value.get + "says : " + this.leftTree.get.value + "is my left child\n"
    } else if (isOnlyRight) ("node" + this.value.get + "says : " + this.rightTree.get.value + "is right my child\n") else if (isInner) {
      "node" + this.value.get + "says : " + this.leftTree.get.value + "is my left child\n" +
        ("node" + this.value.get + "says : " + this.rightTree.get.value + "is right my child\n")
    } else {
      "node" + this.value.get + "says, I am leaf"
    }
  }

  class BinaryTree(rootNodeValue: Option[Int]) {

    val rootNode = Node(rootNodeValue)

    def height = calculateHeight(Some(rootNode), 0)

    def balanced = isbalanced(Some(rootNode))

    def nodes = calculateNumberOfNodes(Some(rootNode), 0)

    override def toString = rootNode.toString

    def traverse(tree: Option[Node]) {
      if (tree.isDefined) {
        if (tree.get.isInner) {
          println(tree.get)
          traverse(tree.get.leftTree)
          traverse(tree.get.rightTree)
        } else if (tree.get.isLeaf)
          println("Leaf" + tree)
      }
    }

    def insert(value: Int, node: Node = rootNode): Unit = if (node.value.isDefined) {
      if (!node.leftTree.isDefined) {
        node.leftTree = Some(Node(Some(value)))
      } else if (!node.rightTree.isDefined) {
        node.rightTree = Some(Node(Some(value)))
      } else if (calculateNumberOfNodes(node.leftTree, 0) < Math.pow(2, calculateHeight(node.leftTree, 0)) - 1)
        insert(value, node.leftTree.get)
      else if (calculateNumberOfNodes(node.leftTree, 0) == calculateNumberOfNodes(node.rightTree, 0))
        insert(value, node.leftTree.get)
      else insert(value, node.rightTree.get)
    }

    def max_hepify(node: Node = rootNode) {
      if (node.isInner) {
        if (node.leftTree.isDefined && node.rightTree.isDefined) {
          if (node.leftTree.get.value.get > node.value.get && node.leftTree.get.value.get > node.rightTree.get.value.get)
            swap(node, node.leftTree.get)
          else if (node.rightTree.get.value.get > node.value.get && node.leftTree.get.value.get < node.rightTree.get.value.get) {
            swap(node, node.rightTree.get)
          }
        } else if (node.leftTree.isDefined && node.leftTree.get.value.get > node.value.get) {
          swap(node, node.leftTree.get)
        } else if (node.rightTree.isDefined && node.rightTree.get.value.get > node.value.get) {
          swap(node, node.rightTree.get)
        }
        if (node.leftTree.isDefined && node.rightTree.isDefined) {
          max_hepify(node.leftTree.get)
          max_hepify(node.rightTree.get)
        }
        if (node.leftTree.isDefined) max_hepify(node.leftTree.get)
        if (node.rightTree.isDefined) max_hepify(node.rightTree.get)
      }
    }

    def buildMaxHeap(node: Option[Node] = Some(rootNode)) {
      if (node.isDefined) {
        if (!node.get.isLeaf) {
          buildMaxHeap(node.get.leftTree)
          buildMaxHeap(node.get.rightTree)
          max_hepify(node.get)
        }
      }
    }

    def sort = {
      buildMaxHeap(Some(rootNode))
      (for (i <- 1 to nodes) yield swapRootAndLastLeafAndExtractMax(rootNode)).mkString(",")
    }

    def swapRootAndLastLeafAndExtractMax(node: Node) = {
      buildMaxHeap(Some(node))
      if (node.isLeaf) node.value.get else {
        println("root is " + node.value)	
        var lastLeaf = lastLeafOfTree(Some(node))
        if (lastLeaf.isDefined) {
          val value = node.value.get
          println("................" + lastLeaf)
          rootNode.value = lastLeaf.get.value
          lastLeaf = None
          println("after root is " + node.value)
          value
        } else node.value.get
      }
    }

    def lastLeafOfTree(node: Option[Node] = Some(rootNode)): Option[Node] = {
      if (calculateNumberOfNodes(node, 0) == 1) node
      else if (node.isDefined) {
        if (node.get.isLeaf) node
        else if (calculateHeight(node.get.leftTree, 0) > calculateHeight(node.get.rightTree, 0))
          lastLeafOfTree(node.get.leftTree) else lastLeafOfTree(node.get.rightTree)
      } else node
    }

    def swap(node1: Node, node2: Node): Unit = {
      val temp = node1.value
      node1.value = node2.value
      node2.value = temp
      //println("swapping", node1.value, node2.value)
    }

    def swap2(node1: Node, node2: Node): Unit = {
      val temp = node1.value
      node1.value = node2.value
      node2.value = temp
      //println("swapping", node1.value, node2.value)
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

  val tree = new BinaryTree(Some(2))
  data.foreach { i => tree.insert(i) }
  println(tree.height)
  println(tree.balanced)
  println(tree.traverse(Some(tree.rootNode)))
  println(tree.sort)
}