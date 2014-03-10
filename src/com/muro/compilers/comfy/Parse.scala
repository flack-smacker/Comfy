/*
 * 
 */

package com.muro.compilers.comfy

object Parse {

  def parse(tokenStream: scala.collection.mutable.Queue[Token]) = {

    var currentToken = tokenStream.dequeue()

    // Kick-off parse
    program()

    /**
     * Parses a program. This method kicks off the parse phase.
     *
     * Program ::== Block $
     */
    def program() = {

      println("Parsing program...")

      block()

      if (!isCurrentTokenValid(Tag.T_endOfProgram))
        println("ERROR: A program should end with the end-of-program marker.")
    }

    def block(): Unit = {

      println("Parsing block...")

      if (!isCurrentTokenValid(Tag.T_openBrace))
        println("ERROR: A block should begin with an opening brace.")

      statementList()

      if (!isCurrentTokenValid(Tag.T_closeBrace))
        println("ERROR: A block should end with an opening brace.")
    }

    /**
     * Parses a sequence of statements.
     *
     * StatementList ::== Statement StatementList
     *               ::== emptyList
     */
    def statementList() = {

      println("Parsing statement list...")

      // Parse the list until a closing brace is encountered.
      while (currentToken.tag != Tag.T_closeBrace)
        statement()
    }

    def statement() = {

      println("Parsing statement...")

      if (currentToken.tag == Tag.T_print)
        printStatement()
      else if (currentToken.tag == Tag.T_id)
        assignmentStatement()
      else if (currentToken.tag == Tag.T_type)
        varDecl()
      else if (currentToken.tag == Tag.T_while)
        whileStatement()
      else if (currentToken.tag == Tag.T_if)
        ifStatement()
      else if (currentToken.tag == Tag.T_openBrace)
        block()
      else
        println("ERROR: Expecting a statement.");
    }

    def printStatement() = {

      println("Parsing print statement...")

      if (!isCurrentTokenValid(Tag.T_print))
        println("ERROR: A print statement should begin with the print keyword.")

      if (!isCurrentTokenValid(Tag.T_openParen))
        println("ERROR: The print keyword should be followed by an opening parenthesis.")

      expression()

      if (!isCurrentTokenValid(Tag.T_closeParen))
        println("ERROR: The print keyword should be followed by a closing parenthesis.")
    }

    /**
     * Parses an assignment statement.
     *
     * AssignmentStatement ::== Id = Expr
     */
    def assignmentStatement() = {

      println("Parsing assignment statement...")

      // Check for an Id
      if (!isCurrentTokenValid(Tag.T_id))
        println("ERROR: Expecting an ID for an assignment statement.")

      // Check for an equals sign.
      if (!isCurrentTokenValid(Tag.T_assignOp))
        println("ERROR: Expecting an assignment operator.")

      // Parse the right-hand side of the assignment.
      expression()
    }

    /**
     * Parses a variable declaration statement.
     *
     * VarDecl ::== type Id
     */
    def varDecl() = {

      println("Parsing variable declaration statement...")

      if (!isCurrentTokenValid(Tag.T_type))
        println("ERROR: Expecting a valid type keyword.")

      if (!isCurrentTokenValid(Tag.T_id))
        println("ERROR: Expecting an identifier.")
    }

    /**
     * Parses a while statement.
     *
     * WhileStatement ::== while BooleanExpr Block
     */
    def whileStatement() = {

      println("Parsing while statement...")

      // A while statement should begin with the while keyword.
      if (!isCurrentTokenValid(Tag.T_while))
        println("ERROR: Expecting \"while\" keyword.")

      // Parse the conditional expression
      booleanExpr()

      // Parse the body of the while loop
      block()
    }

    /**
     * Parse an if statement.
     *
     * IfStatement ::== if BooleanExpr Block
     */
    def ifStatement() = {

      println("Parsing if statement...")

      if (!isCurrentTokenValid(Tag.T_if))
        println("ERROR: Expecting \"if\" keyword.")

      // Parse the conditional expression
      booleanExpr()

      // Parse the body of the if statement
      block()
    }

    def expression(): Unit = {

      println("Parsing expression...")

      if (currentToken.tag == Tag.T_numLiteral)
        intExpr()
      else if (currentToken.tag == Tag.T_stringLiteral)
        stringExpr()
      else if (currentToken.tag == Tag.T_openParen ||
        currentToken.tag == Tag.T_boolLiteral)
        booleanExpr()
      else if (currentToken.tag == Tag.T_id)
        println("id expression")
      else
        println("ERROR: Expecting an expression.")
    }

    /**
     * Parses an int expression.
     *
     * IntExpr ::== digit intop Expr
     */
    def intExpr() = {

      println("Parsing integer expression...")

      // An integer expression begins with...an integer.
      if (!isCurrentTokenValid(Tag.T_numLiteral))
        println("ERROR: Expecting a numeric literal.")

      // Look-ahead to determine whether this is an addition operation.
      if (tokenStream.head.tag == Tag.T_plusOp) {
        currentToken = tokenStream.dequeue
        expression()
      }
    }

    /**
     * Parses a string expression.
     *
     * StringExpr ::== " CharList "
     */
    def stringExpr() = {

      println("Parsing string expression...")

      if (!isCurrentTokenValid(Tag.T_stringLiteral))
        println("ERROR: Expecting a string constant.")
    }

    /**
     * Parses a boolean expresion.
     *
     * BooleanExpr ::== ( Expr boolop Expr )
     *             ::== boolval
     */
    def booleanExpr() = {

      println("Parsing boolean expression...")

      if (tokenStream.head.tag == Tag.T_boolLiteral) {
        // do other parse tree building stuff
        currentToken = tokenStream.dequeue
      } else {

        if (!isCurrentTokenValid(Tag.T_openParen))
          println("ERROR: A boolean expression begins with an open parenthesis.")

        expression()

        if (!isCurrentTokenValid(Tag.T_boolOp))
          println("ERROR: A boolean expression must contain a boolean operator.")

        expression()

        if (!isCurrentTokenValid(Tag.T_closeParen))
          println("ERROR: A boolean expression ends with a closing parenthesis.")
      }
    }

    /**
     * Determines whether the current token matches the specified expected token.
     */
    def isCurrentTokenValid(expected: Tag.Value): Boolean = {

      // Check whether the current token matches the expected token.
      var result: Boolean = currentToken.tag == expected

      println("Expecting..." + expected.toString)
      println("Found..." + currentToken.getDescription)

      // If so, grab the next token from the stream.
      if (result && !tokenStream.isEmpty) {
        currentToken = tokenStream.dequeue
      }

      // Return the result.
      result
    }
  }
}
