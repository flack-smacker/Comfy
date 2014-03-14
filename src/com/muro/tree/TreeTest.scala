/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.tree

object TreeTest {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    var t = new Tree()
    t.insert(new Node("Program"))
    t.insert(new Node("Block"))
    t.insert(new Node("StatementList"))
    t.insert(new Node("Statement"))
    t.insert(new Node("PrintStatement"))
    t.current.children.append(new Node("("))
    t.current.children.append(new Node("Expr"))
    t.current.children.append(new Node(")"))
    print(t.toString)
  }

}
