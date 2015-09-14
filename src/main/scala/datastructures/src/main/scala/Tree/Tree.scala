package datastructures.Tree

/**
 * Created by piyushm on 9/9/15.
 */
case class Node[T](key: Int, data: T) {
  var left: Option[Node[T]] = None
  var right: Option[Node[T]] = None

  def isDefined = Option(this) != None

  def hasLeft = left.isDefined

  def hasRight = right.isDefined

  def hasOnlyRight = hasLeft & !hasRight

  def hasOnlyLeft = !hasLeft & hasRight

  def isLeaf = this.isDefined && (!hasLeft && !hasRight)

  def isInner = this.isDefined && (hasLeft || hasRight)

  def isNonLeaf = !isLeaf

  override def toString = key.toString
}


class Tree[T](properties: Map[String, String]) {
  var rootNode: Option[Node[T]] = None

  def genericInsert(key: Int, data: T, currentNode: Option[Node[T]] = rootNode): Unit = {
    if (properties.getOrElse("tree.BST", "false") == "true") {
      /* a binary tree is a tree data structure in which
       each node has at most two children, which are referred
       to as the left child and the right child.*/
      addNode(key, data, currentNode)
    } else if (properties.getOrElse("tree.BalanceBST", "false") == "true") {
    }
  }


  def addNode(key: Int, data: T, currentNode: Option[Node[T]] = rootNode): Unit = {
    if (!rootNode.isDefined) {
      rootNode = Some(Node(key, data))
      println("adding root")
    } else if (!currentNode.get.hasLeft && key < currentNode.get.key) {
      currentNode.get.left = Some(Node(key, data))
      println(key + " left child of " + currentNode)
    } else if (!currentNode.get.hasRight && key > currentNode.get.key) {
      currentNode.get.right = Some(Node(key, data))
      println(key + " right child of " + currentNode)
    } else if (key > currentNode.get.key) {
      addNode(key, data, currentNode.get.right)
    } else {
      addNode(key, data, currentNode.get.left)
    }
  }

  def comapre(firstTree: Option[Node[T]], secondTree: Option[Node[T]]): Boolean = {
    if (firstTree.isDefined && secondTree.isDefined) {
      (firstTree.get.data == secondTree.get.data && comapre(firstTree.get.left, secondTree.get.left)
        && comapre(firstTree.get.right, secondTree.get.right))
    } else if (firstTree.isDefined == secondTree.isDefined) true
    else
      false
  }

  def preOrder(node: Option[Node[T]] = rootNode): Unit = {
    if (node.isDefined) {
      print(node.get.data + " ")
      preOrder(node.get.left)
      preOrder(node.get.right)
    }
  }

  def postOrder(node: Option[Node[T]] = rootNode): Unit = {
    if (node.isDefined) {
      postOrder(node.get.left)
      postOrder(node.get.right)
      print(node.get.data + " ")
    }
  }

  def inOrder(node: Option[Node[T]] = rootNode): Unit = {
    if (node.isDefined) {
      inOrder(node.get.left)
      print(node.get.data + " ")
      inOrder(node.get.right)
    }
  }

  def size(node: Option[Node[T]] = rootNode): Int = {
    if (node.isDefined) {
      1 + size(node.get.left) + size(node.get.right)
    } else 0
  }

  def traverse(currentNode: Option[Node[T]] = rootNode): Unit = {
    if (currentNode.isDefined) {
      if (currentNode.get.isLeaf) println("Leaf : " + currentNode)
      else {
        println("Node: " + currentNode.get)
        traverse(currentNode.get.left)
        traverse(currentNode.get.right)
      }
    } else println("")
  }
}


object TreeTest extends App {
  val tree = new Tree[Integer](Map("tree.BST" -> "true"))
  val tree1 = new Tree[Integer](Map("tree.BST" -> "true"))
  val tree2 = new Tree[Integer](Map("tree.BST" -> "true"))

  val array = Array(13, 3, 4, 12, 14, 10, 5, 1, 8, 2, 7, 9, 11, 6, 18)
  val array1 = Array(2, 1, 3)

  val array2 = Array(2, 3, 1)

  array foreach { a => tree.genericInsert(a, a) }
  array1 foreach { a => tree1.genericInsert(a, a) }
  array2 foreach { a => tree2.genericInsert(a, a) }

  tree.preOrder()
  println(" ")
  tree.postOrder()
  println(" ")
  tree.inOrder()
  println("\nsize " + tree.size())
  println(tree1.comapre(tree1.rootNode, tree2.rootNode))


  //
}