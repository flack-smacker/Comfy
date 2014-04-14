/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

import com.muro.tree._

object Analyzer {

  /**
   * Constructs an AST from the specified CST.
   */
  def constructAST(cst: Tree): Tree = {

    // Create an initially empty AST.
    var ast: Tree = new Tree()

    /**
     * Performs a depth-first in-order traversal of the specified tree.
     */
    def depthFirstInOrder(cst: Tree): Unit = {

      // Do we have a tree?
      if (cst == null || cst.root == null)
        return

      // Start with the root node.
      doTraverse(cst.root)
    }

    /**
     * Performs a recursive, depth-first in-order traversal of the tree starting
     * at the specified node.
     */
    def doTraverse(aNode: Node): Unit = {

      def block(): Unit = {
        println("Found Block")

        // Add a new node to the AST
        ast.insert(new Node("Block", ""))

        // Create new environment
      }

      def print(): Unit = {
        println("Found Print")

        // Add a new node to the AST.
        cst.insert(new Node("Print", ""))

        // Parse the print Expression

      }

      def assignment(): Unit = {
        println("Found Assignment")

        // Add a new node to the AST.
        ast.insert(new Node("AssignmentStatement", ""))

        // Add the identifier to the AST
        ast.current.children.append(aNode.children.head);
        
        // Add the expression tree...
      }

      def loop(): Unit = {
        println("Found Loop")
      }

      def declaration(): Unit = {
        println("Found Declaration")
        
        // Add a new node to the AST.
        ast.insert(new Node("VarDecl"))
        
        // Add the variable type to the AST.
        ast.current.children.append(aNode.children.head);
        
        // Add the variable name to the AST.
        
      }

      def conditional(): Unit = {
        println("Found Conditional")
      }

      // Identify the node type and take the appropriate action.
      aNode.label.toString match {

        case "Block" => {
          block()
        }

        case "IfStatement" => {
          conditional()
        }

        case "VarDecl" => {
          declaration()
        }

        case "WhileStatement" => {
          loop()
        }

        case "PrintStatement" => {
          print()
        }

        case _ => {
          println("No Match found.")
        }
      }

      // Ok, we've identified this node. Now identify its' children.
      aNode.children.foreach(doTraverse)
    }

    depthFirstInOrder(cst)
    null
  }
}
