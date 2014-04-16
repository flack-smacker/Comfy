/*
 *
 */

package com.muro.compilers.comfy

import com.muro.compilers.comfy.exceptions.InvalidExpression
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
    def construct(): Unit = {

      // Do we have a tree?
      if (cst == null || cst.root == null)
        return

      // Start with the root node.
      visit(cst.root)
    }

    def visit(toVisit: Node): Unit = {      
      // Process the current node.
      process(toVisit)
      // Process the node's children, starting with the leftmost child node.
      toVisit.children.foreach(visit)
    }
    
    // Identify the node type and take the appropriate action.
    def process(node: Node): Unit = {

      node.label.toString match {

        case "Block" => {
            block()
          }

        case "IfStatement" => {
            conditional(node)
          }

        case "VarDecl" => {
            declaration(node)
          }

        case "WhileStatement" => {
            loop()
          }

        case "PrintStatement" => {
            print(node)
          }
          
        case "AssignmentStatement" => {
            assignment(node)
        }

        case _ => {
            // do nothing?
          }
      }


      def block(): Unit = {
        println("Found Block")

        // Add a new node to the AST
        ast.insert(new Node("Block"))
        
        // Create new environment
      }

      def print(node: Node): Unit = {
        
        println("Found Print")

        // Add a new node to the AST.
        ast.insert(new Node("Print"))

        // Analyze the expression to be printed.
        expression(node.children(2).children(0))
        
        // Reset the current node.
        ast.current = ast.current.parent
      }

      def assignment(node: Node): Unit = {
        println("Found Assignment")

        // Add a new node to the AST.
        ast.insert(new Node("Assignment"))

        // Add the identifier to the AST
        ast.current.children.append(
          new Node(node.children(0).children(0).label.toString))
        
        // Add the expression tree...
        expression(node.children(2).children(0))
        
        // Reset the current node.
        ast.current = ast.current.parent
      }

      def loop(): Unit = {
        println("Found Loop")
      }

      def declaration(node: Node): Unit = {
        
        println("Found Declaration")

        // Add a new node to the AST.
        ast.insert(new Node("VarDecl"))

        // Add the variable type to the AST.
        ast.current.children.append(
          new Node(node.children(0).children(0).label.toString))
        // Add the variable name to the AST.
        ast.current.children.append(
          new Node(node.children(1).children(0).label.toString))
        
        // Move the current pointer back up the tree.
        ast.current = ast.current.parent
      }

      def conditional(node: Node): Unit = {
        println("Found Conditional")
      }
      
      /**
       * Parses an expression.
       *
       * Expr ::== IntExpr
       *      ::== StringExpr
       *      ::== BooleanExpr
       *      ::== Id
       */
      def expression(exprNode: Node): Unit = {

        println("Building expression tree...")

        exprNode.label.toString match {
            
          case "IntExpr" => {
              IntExpr(exprNode)
            }
            
          case "StringExpr" => {
              StringExpr(exprNode)
            }
          
          case "BooleanExpr" => {
              BooleanExpr(exprNode)
            }
            
          case "Id" => {
              IdExpr(exprNode)
            }
        }
        
        def IntExpr(node: Node): Unit = {
          
          // An integer expression consisting of multiple digits.
          if (node.children.size > 1) {
            
            // Insert the terminal corresponding to the integer operation.
            ast.insert(
              new Node(node.children(1).children(0).label.toString))
            
            // Insert the digit on the left handside.
            ast.current.children.append(
              new Node(node.children(0).children(0).label.toString))
            
            // Make sure the expression on the right-hand side is an IntExpr
            if (node.children(2).children(0).label.toString != "IntExpr")
              throw new InvalidExpression("Expecting integer expression.")
            else
              expression(node.children(2).children(0))
            
            // Reset the current node.
            ast.current = ast.current.parent
          } else {
            // This is an integer expression consisting of a single digit.
            ast.current.children.append(
              new Node(node.children(0).children(0).label.toString))
          }
        }
        
        def StringExpr(node: Node): Unit = {
          ast.current.children.append(
            new Node(node.children(1).children(0).label.toString))
        }
        
        def BooleanExpr(node: Node): Unit = {
          
        }
        
        def IdExpr(node: Node): Unit = {
          ast.current.children.append(
            new Node(node.children(0).label))
        }
      }
    }
    
    // Kick-off anaylsis.
      construct()
   
    // Return the resulting AST
    ast
  }
}
  
