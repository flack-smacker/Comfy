/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.tree

import scala.collection.mutable.LinkedList

class Node[T](var data:T, val children:LinkedList[T])

class Tree[T](val r:Node[T]) {
  val root = r
  
  def addChild(c: Node[T]) {
    
  }
}