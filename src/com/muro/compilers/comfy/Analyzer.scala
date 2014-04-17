/**
 * Constructs an AST using a sequence of nodes generated during parse. Performs type-checking while constructing
 * the AST.
 */

package com.muro.compilers.comfy

import com.muro.compilers.comfy.exceptions.TypeMismatchException
import com.muro.tree._

object Analyzer {

  /**
   * Constructs an AST from the specified CST.
   */
  def constructAST(astNodes: scala.collection.mutable.Queue[Node]): Tree = {

    // Create an initially empty AST.
    var ast: Tree = new Tree()
    
    // Identify the node type and take the appropriate action.
    def process(node: Node) {

      node.label match {

        case "Block" => {
            block()
          }
          
        case "EndBlock" => {
            exitBlock()
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


      def block() {
        // Output Status.
        println("Found Block")
        // Add a new node to the AST
        ast.insert(new Node("Block"))
        
        // Check if we are entering the first (global) block...
        // If we arent, that is, if this is a nested block then we have to 
        // follow along in the symbol table by entering the current scopes next 
        // nested scope. (It doesn't make sense to me either.)
        if (ast.current.parent != null) 
          SymbolTable.currentEnv = SymbolTable.currentEnv.children.head
      }
      
      def exitBlock() {
        
        // Output Status.
        println("Exiting Block")
        // Move the current pointer up to the nearest containing block.
        ast.current = ast.current.parent
        
        // This is a hack that handles the case where we process an If/While node.
        // These nodes contain inner blocks that we need to break out of.
        while (ast.current != null && ast.current.label != "Block")
          ast.current = ast.current.parent
        
        // Delete the current environment.
        // This method causes the parent environment to become the current.
        SymbolTable.delete()
      }

      def print(node: Node) {
        
        // Output Status.
        println("Found Print")

        // Add a new node to the AST.
        ast.insert(new Node("Print"))

        // Analyze the expression to be printed.
        expression(node.children(2).children(0))
        
        // Move the current pointer up to the containing block.
        ast.current = ast.current.parent
      }

      def assignment(node: Node) {
        
        // Output Status.
        println("Found Assignment")

        // Add a new node to the AST.
        ast.insert(new Node("Assignment"))

        // Extract the identifier from the node.
        val id = node.children(0).children(0).label
        // Add the identifier to the AST
        ast.current.children.append(new Node(id))
        
        // Extract the identifier's information from the symbol table.
        val entry = SymbolTable.lookup(id)
        
        // Extract the identifier's declared type from the symbol table.
        val declType = entry.idType
        
        // Add the expression tree...
        val expType = expression(node.children(2).children(0))
        
        if (declType != expType)
          throw new TypeMismatchException("Cannot assign " + expType + 
          " value to " + declType + " declared on line " + entry.line)
        
        // Move the current pointer up to the containing block.
        ast.current = ast.current.parent
      }

      def loop() {
        // Output Status.
        println("Found While Statement")
        // Add a new node to the AST.
        ast.insert(new Node("While"))

        // Build the expression tree for the conditional expression.
        val expType = expression(node.children(1))
        
        if (expType != ComfyType.Boolean)
          throw new TypeMismatchException(
            "Type " + expType + " found in While statement condition. " +
            "Conditional expression must evaluate to a boolean value.")
        
        // The body of the while loop is processed as the next node on the queue.
      }

      def declaration(node: Node) {
        
        // Output Status.
        println("Found Declaration")
        // Add a new node to the AST.
        ast.insert(new Node("VarDecl"))

        // Add the variable type to the AST.
        ast.current.children.append(
          new Node(node.children(0).children(0).label))
        // Add the variable name to the AST.
        ast.current.children.append(
          new Node(node.children(1).children(0).label))
        
        // Move the current pointer up to the containing block.
        ast.current = ast.current.parent
      }

      def conditional(node: Node) {
        
        // Output Status.
        println("Found If Statement")
        // Add a new node to the AST.
        ast.insert(new Node("If"))

        // Build the expression tree for the conditional expression.
        val expType = expression(node.children(1))
        
        if (expType != ComfyType.Boolean)
          throw new TypeMismatchException(
            "Type " + expType + " found in If statement condition. " +
            "Conditional expression must evaluate to a boolean value.")
      }
      
      /**
       * Parses an expression.
       *
       * Expr ::== IntExpr
       *      ::== StringExpr
       *      ::== BooleanExpr
       *      ::== Id
       */
      def expression(exprNode: Node): String = {

        // Output Status.
        println("Building expression tree...")

        var toReturn = ""
        
        exprNode.label match {
            
          case "IntExpr" => {
              IntExpr(exprNode)
              toReturn = ComfyType.Int
            }
            
          case "StringExpr" => {
              StringExpr(exprNode)
              toReturn = ComfyType.String
            }
          
          case "BooleanExpr" => {
              BooleanExpr(exprNode)
              toReturn = ComfyType.Boolean
            }
            
          case "Id" => {
              toReturn = IdExpr(exprNode)
            }
        }
        
        def IntExpr(node: Node) {
          
          // An integer expression consisting of multiple digits.
          if (node.children.size > 1) {
            
            // Insert the terminal corresponding to the integer operation.
            ast.insert(
              new Node(node.children(1).children(0).label))
            
            // Insert the digit on the left handside.
            ast.current.children.append(
              new Node(node.children(0).children(0).label))
            
            // Evaluate the expression on the right-hand side.
            val expType = expression(node.children(2).children(0))
            
            if (expType != ComfyType.Int)
              throw new TypeMismatchException(
                "Expecting an integer to complete integer expression on line.")
            
            // Reset the current node.
            ast.current = ast.current.parent
          } else {
            // This is an integer expression consisting of a single digit.
            ast.current.children.append(
              new Node(node.children(0).children(0).label))
          }
        }
        
        def StringExpr(node: Node) {
          ast.current.children.append(
            new Node(node.children(1).children(0).label))
        }
        
        def BooleanExpr(node: Node) {
          
          // Is this a boolean literal?
          if (node.children.length == 1) {
            ast.insertChild(node.children(0).children(0))
          } else {
            // Insert the boolean operation.
            ast.insert(node.children(2).children(0))
            
            // Evaluate the expression on the left-hand side
            val expType1 = expression(node.children(1).children(0))
            
            // Evaluate the expression on the right-hand side.
            val expType2 = expression(node.children(3).children(0))
            
            if (expType1 != expType2)
              throw new TypeMismatchException(
                "Cannot perform comparison of values of differing types.")
              
            // Reset the current pointer.
            ast.current = ast.current.parent
          }
        }
        
        def IdExpr(node: Node): String = {
          ast.current.children.append(
            new Node(node.children(0).label))
          // Return the type of this id.
          SymbolTable.lookup(node.children(0).label).idType
        }
        
        toReturn
      }
      
    }
    
    // Kick-off construction.
    astNodes.foreach((node: Node) => process(node))
   
    // Return the resulting AST
    ast
  }
}
