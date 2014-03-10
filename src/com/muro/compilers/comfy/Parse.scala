/*
 * 
 */

package com.muro.compilers.comfy

object Parse {
  
  def parse(tokenStream: scala.collection.mutable.Queue[Token]) = {
    
    var currentToken = tokenStream.dequeue()
    
    def program() = {
      block()
    }
    
    def block():Unit = {
      
      if (!isCurrentTokenValid(Tag.T_openBrace))
        println("ERROR: A block should begin with an opening brace.")
      
      statementList()
      
      if (!isCurrentTokenValid(Tag.T_closeBrace))
        println("ERROR: A block should end with an opening brace.")
    }
    
    def statementList() = {
      statement()
    }
    
    def statement() = {
      if (tokenStream.head.tag == Tag.T_print)
        printStatement()
      else if (tokenStream.head.tag == Tag.T_id)
        assignmentStatement()
      else if (tokenStream.head.tag == Tag.T_type)
        varDecl()
      else if (tokenStream.head.tag == Tag.T_while)
        whileStatement()
      else if (tokenStream.head.tag == Tag.T_if)
        ifStatement()
      else if (tokenStream.head.tag == Tag.T_openBrace)
        block()
      else
        println("ERROR: Expecting a statement.");
    }
    
    def printStatement() = {
      
      if (currentToken.tag == Tag.T_openBrace)
        currentToken = tokenStream.dequeue
      else
        println("ERROR: A block should begin with an opening brace.")
      
      expression()
      
      if (currentToken.tag == Tag.T_closeParen)
        currentToken = tokenStream.dequeue
      else
        println("ERROR: A block should end with an opening brace.")
    }
    
    /**
     * Parses an assignment statement.
     * AssignmentStatement ::== Id = Expr
     */
    def assignmentStatement() = {
      
      // Check for an Id
      if (currentToken.tag == Tag.T_id)
        currentToken = tokenStream.dequeue
      else
        println("ERROR: Expecting an ID for an assignment statement.")
      
      // Check for an equals sign.
      if (currentToken == Tag.T_assignOp)
        currentToken = tokenStream.dequeue
      else
        println("ERROR: Expecting an assignment operator.")
      
      // Parse the right-hand side of the assignment.
      expression()
    }
    
    /**
     * Parses a variable declaration statement.
     * VarDecl ::== type Id
     */
    def varDecl() = {
      if (currentToken.tag == Tag.T_type)
        currentToken = tokenStream.dequeue
      else
        println("ERROR: Expecting a valid type keyword.")
      
      if (currentToken.tag == Tag.T_id)
        currentToken = tokenStream.dequeue
      else
        println("ERROR: Expecting an identifier.")
    }
    
    /**
     * Parses a while statement.
     * WhileStatement ::== while BooleanExpr Block
     */
    def whileStatement() = {
      if (currentToken.tag == Tag.T_while)
        currentToken = tokenStream.dequeue
      else
        println("ERROR: Expecting \"while\" keyword.")
      
      // Parse the conditional expression
      booleanExpr()
      
      // Parse the body of the while loop
      block()
    }
    
    /**
     * Parse an if statement.
     * IfStatement ::== if BooleanExpr Block
     */
    def ifStatement() = {
      if (currentToken.tag != Tag.T_if)
        println("ERROR: Expecting \"if\" keyword.")
      
      // We have an if keyword. Grab the next token from the stream.
      currentToken = tokenStream.dequeue
      
      // Parse the conditional expression
      booleanExpr()
      
      // Parse the body of the if statement
      block()
    }
    
    def expression():Unit = {
      if (currentToken.tag == Tag.T_numLiteral)
        intExpr()
      else if (currentToken.tag == Tag.T_string)
        stringExpr()
      else if (currentToken.tag == Tag.T_openParen)
        booleanExpr()
      else if (currentToken.tag == Tag.T_id)
        println("id expression")
      else 
        println("ERROR: Expecting an expression.")
    }
    
    /**
     * Parses an int expression.
     * IntExpr ::== digit intop Expr
     */
    def intExpr() = {
      
      // An integer expression begins with...an integer.
      if (currentToken.tag != Tag.T_numLiteral)
        println("ERROR: Expecting a numeric constant.")
      
      // Grab the next token from the stream.
      currentToken = tokenStream.dequeue
      
      // Check whether this is an addition operation.
      if (currentToken.tag == Tag.T_plusOp)
        expression()
    }
    
    /**
     * Parses a string expression.
     * StringExpr ::== " CharList "
     */
    def stringExpr() = {
      if (currentToken.tag != Tag.T_string)
        println("ERROR: Expecting a string constant.")
    }
    
    /**
     * Parses a boolean expresion.
     * BooleanExpr ::== ( Expr boolop Expr )
     */
    def booleanExpr() = {
      if (currentToken.tag != Tag.T_string)
        println("ERROR: Expecting a string constant.")
    }
    
    def anIdentifier() = {
      
    }
    
    def charList() = {
      
    }
    
    def aType() = {
      
    }
    
    def aChar() = {
      
    }
    
    def aSpace() = {
      
    }
    
    def aDigit() = {
      
    }
    
    def boolOperation() = {
      
    }
    
    def boolValue() = {
      
    }
    
    def intOperation() = {
      
    }
    
    /**
     * Determines whether the current token matches the specified expected token.
     */
    def isCurrentTokenValid(expected: Tag):Boolean = {
      
      // Check whether the current token matches the expected token.
      var result: Boolean = currentToken.tag == expected
      
      // If so, grab the next token from the stream.
      if (result) {
        currentToken = tokenStream.dequeue
      }
      
      // Return the result.
      result
    }
  }
}
