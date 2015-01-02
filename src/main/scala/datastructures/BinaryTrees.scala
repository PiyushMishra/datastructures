package datastructures

object module {

  abstract class Tree {
    val leftTree: Option[Tree]
    val rightTree: Option[Tree]
    def isLeaf: Boolean
  }

  case class Leaf(value: Int) extends Tree {
    val leftTree: Option[Tree] = None
    val rightTree: Option[Tree] = None
    def isLeaf = true
  }

  case class SubTree(value: Int, leftTree: Option[Tree] = None, rightTree: Option[Tree]) extends Tree {
    def isLeaf = false
  }
  
  val data = Array(1, 2, 3, 4, 5, 6, 7)

  println()

}