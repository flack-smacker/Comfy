/*
 * 
 */
package com.muro.tree

import scala.collection.mutable.ListBuffer

class Node (val label: String, val terminal:String = "") {
  
  /**
   * A list containing the nodes that appear as children of this node.
   */
  var children: ListBuffer[Node] = new ListBuffer()
  
  /**
   * A reference to this node's parent node.
   */
  var parent: Node = null
}

