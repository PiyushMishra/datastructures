package datastructures

object module {

  abstract class TreeNode {
    val leftTree: Option[TreeNode]
    val rightTree: Option[TreeNode]
    def isLeaf: Boolean
  }

  case class Leaf(value: Int) extends TreeNode {
    val leftTree: Option[TreeNode] = None
    val rightTree: Option[TreeNode] = None
    def isLeaf = true
  }

  case class SubTree(value: Int, leftTree: Option[TreeNode] = None, rightTree: Option[TreeNode]) extends TreeNode {
    def isLeaf = false
  }
  
  
  class BinaryTree {
    
    
    
  }
  
  val data = Array(1, 2, 3, 4, 5, 6, 7)

  println()

}