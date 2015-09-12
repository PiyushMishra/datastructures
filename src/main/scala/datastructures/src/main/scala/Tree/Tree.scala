package datastructures.Tree

import java.util.logging.Logger


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

  def isInner = this.isDefined && hasLeft && hasRight

  def isNonLeaf = !isLeaf

  override def toString = if (hasOnlyLeft) {
    "node" + this.key + "says : " + this.left.get.key + "is my left child\n"
  } else if (hasOnlyRight) ("node" + this.key + "says : " +
    this.right.get.key + "is right my child\n")
  else if (isInner) {
    "node" + this.key + "says : " + this.left.get.key + "is my left child\n" +
      ("node" + this.key + "says : " + this.right.get.key + "is right my child\n")
  } else {
    "node" + this.key + "says, I am leaf"
  }
}


class Tree[T](properties: Map[String, String]) extends Logger {
  var rootNode: Option[Node[T]] = None

  if (properties.getOrElse("tree.BST", false) == true) {
    /* a binary tree is a tree data structure in which
     each node has at most two children, which are referred
     to as the left child and the right child.*/
  }

  def addNode(key: Int, data: T, currentNode: Option[Node[T]] = rootNode): Boolean = {
    if (!rootNode.isDefined) {
      rootNode = Some(Node(key, data))
    } else if (!currentNode.get.hasLeft && key < currentNode.get.key) {
      currentNode.get.left = Some(Node(key, data))
    } else if (!currentNode.get.hasRight && key > currentNode.get.key) {
      currentNode.get.right = Some(Node(key, data))
    } else if (key > currentNode.get.key) {
      (key, data, currentNode.get.right)
    } else {
      (key, data, currentNode.get.left)
    }
    true
  }

  def traverse(currentNode: Option[Node[T]] = rootNode): Unit = {
    if (currentNode.isDefined) {
      println(currentNode.get)
      traverse(currentNode.get.left)
      traverse(currentNode.get.right)
    } else println("")
  }
}

object TreeTest extends App {
  val tree = new Tree[Integer](Map())
  tree.addNode(1, 1)
  tree.addNode(2, 2)
  tree.addNode(3, 3)
  tree.addNode(4, 4)
  tree.addNode(5, 5)
  tree.addNode(6, 6)
  tree.addNode(7, 6)
  println(tree.rootNode)
  //  tree.traverse()
}