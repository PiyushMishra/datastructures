package datastructures

object module {

  abstract class TreeNode(val value: Option[Int], val leftTree: Option[TreeNode], val rightTree: Option[TreeNode]) {
    def isLeaf: Boolean
  }

  case class Leaf(override val value: Option[Int], override val leftTree: Option[TreeNode] = None,
    override val rightTree: Option[TreeNode] = None)
    extends TreeNode(value, leftTree, rightTree) {
    def isLeaf = true
  }

  case class SubTree(override val value: Option[Int] = None, override val leftTree: Option[TreeNode] = None,
    override val rightTree: Option[TreeNode] = None)
    extends TreeNode(value, leftTree, rightTree) {
    def isLeaf = false
  }

  class BinaryTree {
    val treeNode: TreeNode = new SubTree

    def insert(value: Int) = {
      if (!treeNode.value.isDefined)
        SubTree(Some(value), treeNode.leftTree, treeNode.rightTree)
      else if (!treeNode.leftTree.isDefined) {
        SubTree(treeNode.value, Some(SubTree(Some(value), None, None)), treeNode.rightTree)
      } else SubTree(treeNode.value, treeNode.leftTree, Some(SubTree(Some(value), None, None)))
    }
  }

  val data = Array(1, 2, 3, 4, 5, 6, 7)

  println()

}