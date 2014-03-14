/*
 * Adapted from Alan Labouseur's javascript
 * implementation of a general tree.
 */

package com.muro.tree

class Tree() {
  
  var root: Node = null
  var current: Node = null
  
  /**
   * Inserts the specified node into this tree.
   */
  def insert(toAdd: Node):Node = {  
    if (root == null)
      root = toAdd
    else
      current.children.append(toAdd)
    
    // The current node is the inserted node's parent.
    toAdd.parent = this.current;
    // The inserted node is now the current node.
    current = toAdd
    
    // Return the newly inserted node.
    toAdd
  }
  
  // Return a string representation of the tree.
  override def toString():String = {
    
    // Initialize the result string.
    var traversalResult = new StringBuilder();

    // Recursive function to handle the expansion of the nodes.
    def expand(node: Node, depth: Int):Unit = {
      
      // Space out based on the current depth so
      // this looks at least a little tree-like.
      for ( _ <- 0 to depth) {
        traversalResult.append("  ")
      }

      // If there are no children (i.e., leaf nodes)...
      if (node.children.isEmpty) {
        // ... note the leaf node.
        traversalResult.append("[" + node.name + "]\n")
      } else {
        // There are children, so note these interior/branch nodes and ...
        traversalResult.append("<" + node.name + "> \n")
        // .. recursively expand them.
        for ( n <- node.children) {
          expand(n, depth + 1);
        }
      }
    }
    
    // Make the initial call to expand from the root.
    expand(this.root, 0);
    
    // Return the result.
    return traversalResult.toString;
  }
}