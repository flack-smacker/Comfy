/*
 * 
 */
package com.muro.tree

import scala.collection.mutable.ListBuffer

class Node (var name:Any) {
  var children:ListBuffer[Node] = new ListBuffer()
  var parent:Node = null
}

