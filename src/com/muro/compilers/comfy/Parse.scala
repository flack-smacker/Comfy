
package com.muro.compilers.comfy

import com.muro.compilers.comfy.exceptions.ParseException
import com.muro.compilers.comfy.grammar._

import com.muro.tree._

/**
 * Parse is responsible for building a parse tree from the specified sequence
 * of tokens.
 */
object Parse {

  def parse(tokenStream: scala.collection.mutable.Queue[Token]): Tree = {

    /**
     * A reference to the token currently being parsed.
     */
    var currentToken = tokenStream.dequeue()

    /**
     * The concrete syntax tree resulting from parse.
     */
    var parseTree: Tree = new Tree()
    
    
    var astNodes = new scala.collection.mutable.Queue[Node]

    /**
     * Parses a program. This method kicks off the parse phase.
     *
     * Program ::== Block $
     */
    def program() = {

      println("Parsing program...")

      var head = parseTree.insert(new Node(Production.Program))

      block()

      if (isCurrentTokenValid(Tag.T_endOfProgram))
        head.children.append(new Node(Terminal.EndOfProgram))
      else
        throw new ParseException(
          "A program should end with the end-of-program marker.")
    }

    /**
     * Parses a block.
     *
     * Block ::== { StatementList }
     */
    def block(): Unit = {

      println("Parsing block...")

      // As a result of this call, Block is the current node.
      var head = parseTree.insert(new Node(Production.Block))

      if (isCurrentTokenValid(Tag.T_openBrace))
        head.children.append(new Node(Terminal.OpenBrace))
      else
        throw new ParseException("A block should begin with an opening brace.")

      statementList()

      if (isCurrentTokenValid(Tag.T_closeBrace))
        head.children.append(new Node(Terminal.CloseBrace))
      else
        throw new ParseException("A block should end with an closing brace.")
      
      astNodes.enqueue(new Node("Block"))
    }

    /**
     * Parses a sequence of statements.
     *
     * StatementList ::== Statement StatementList
     *               ::== emptyList
     */
    def statementList() = {

      println("Parsing statement list...")

      // Insert the StatementList node into the parse tree...
      parseTree.insert(new Node(Production.StatementList))

      // Parse the list until a closing brace is encountered.
      while (currentToken.tag != Tag.T_closeBrace) {
        statement()
        parseTree.current = parseTree.current.parent
      }
    }

    /**
     * Parses a statement.
     *
     * Statement ::== PrintStatement
     *           ::== AssignmentStatement
     *           ::== VarDecl
     *           ::== WhileStatement
     *           ::== IfStatement
     *           ::== Block
     */
    def statement() = {

      println("Parsing statement...")

      parseTree.insert(new Node(Production.Statement))

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
        throw new ParseException("Expecting a statement.");
    }

    /**
     * Parses a print statement.
     *
     * PrintStatement ::== print ( Expr )
     */
    def printStatement() = {

      println("Parsing print statement...")

      var head = parseTree.insert(new Node(Production.PrintStatement))

      if (isCurrentTokenValid(Tag.T_print))
        head.children.append(new Node(Terminal.Print))
      else
        throw new ParseException(
          "A print statement should begin with the print keyword.")

      if (isCurrentTokenValid(Tag.T_openParen))
        head.children.append(new Node(Terminal.OpenParen))
      else
        throw new ParseException(
          "The print keyword should be followed by an opening parenthesis.")

      expression()

      if (isCurrentTokenValid(Tag.T_closeParen))
        head.children.append(new Node(Terminal.CloseParen))
      else
        throw new ParseException(
          "A print statement should end with a closing parenthesis.")
      
      astNodes.enqueue(head)
    }

    /**
     * Parses an assignment statement.
     *
     * AssignmentStatement ::== Id = Expr
     */
    def assignmentStatement() = {

      println("Parsing assignment statement...")
      var head = parseTree.insert(new Node(Production.AssignmentStatement))

      // Check for an Id
      idExpr()

      // Check for an equals sign.
      if (isCurrentTokenValid(Tag.T_assignOp))
        head.children.append(new Node(Terminal.AssignmentOp))
      else
        throw new ParseException("Missing assignment operator.")

      // Parse the right-hand side of the assignment.
      expression()
      
      astNodes.enqueue(head)
    }

    /**
     * Parses a variable declaration statement.
     *
     * VarDecl ::== type Id
     */
    def varDecl() = {

      println("Parsing variable declaration statement...")
      parseTree.insert(new Node(Production.VarDecl))

      if (currentToken.tag == Tag.T_type)
        parseTree.insert(new Node(Production.Type))
      else
        throw new ParseException(
          "A variable declaration should begin with a valid type.")

      // Add the type specifier to the tree
      parseTree.insert(new Node(currentToken.attr))

      // Move the current pointer back up to the VarDecl node.
      parseTree.current = parseTree.current.parent.parent

      // Grab the next token for processing...
      currentToken = tokenStream.dequeue

      idExpr()
      
      astNodes.enqueue(parseTree.current)
    }

    /**
     * Parses a while statement.
     *
     * WhileStatement ::== while BooleanExpr Block
     */
    def whileStatement() = {

      println("Parsing while statement...")
      var head = parseTree.insert(new Node(Production.WhileStatement))

      // A while statement should begin with the while keyword.
      if (isCurrentTokenValid(Tag.T_while))
        head.children.append(new Node(Terminal.While))
      else
        throw new ParseException(
          "A while loop should begin with the \"while\" keyword.")

      // Parse the conditional expression
      booleanExpr()

      // Parse the body of the while loop
      block()
      
      astNodes.enqueue(head)
    }

    /**
     * Parse an if statement.
     *
     * IfStatement ::== if BooleanExpr Block
     */
    def ifStatement() = {

      println("Parsing if statement...")
      var head = parseTree.insert(new Node(Production.IfStatement))

      if (isCurrentTokenValid(Tag.T_if))
        head.children.append(new Node(Terminal.If))
      else
        throw new ParseException(
          "An if statement should begin with the \"if\" keyword.")

      // Parse the conditional expression
      booleanExpr()

      // Parse the body of the if statement
      block()
      
      astNodes.enqueue(head)
    }

    /**
     * Parses an expression.
     *
     * Expr ::== IntExpr
     *      ::== StringExpr
     *      ::== BooleanExpr
     *      ::== Id
     */
    def expression(): Unit = {

      println("Parsing expression...")

      var head = parseTree.insert(new Node(Production.Expr))

      if (currentToken.tag == Tag.T_numLiteral)
        intExpr()
      else if (currentToken.tag == Tag.T_dblQuote)
        stringExpr()
      else if (currentToken.tag == Tag.T_openParen ||
        currentToken.tag == Tag.T_boolLiteral)
        booleanExpr()
      else if (currentToken.tag == Tag.T_id) {
        println("Found identifier expression")
        idExpr()
      } else {
        throw new ParseException("Expecting an expression.")
      }

      // Move the current pointer up to the parent.
      parseTree.current = parseTree.current.parent
    }

    /**
     * Parses an int expression.
     *
     * IntExpr ::== digit intop Expr
     */
    def intExpr() = {

      println("Parsing integer expression...")

      var head = parseTree.insert(new Node(Production.IntExpr))

      // An integer expression begins with...an integer.
      if (currentToken.tag == Tag.T_numLiteral)
        parseTree.insert(new Node(Production.Digit))
      else
        throw new ParseException(
          "An integer expression should begin with a numeric literal.")

      // Insert the actual digit terminal into the tree.
      parseTree.insert(new Node(currentToken.attr))

      // Move the the current node back up the tree two levels.
      parseTree.current = parseTree.current.parent.parent
      // Prepare the next token for processing...
      currentToken = tokenStream.dequeue

      // Determine whether this is an addition expression.
      if (currentToken.tag == Tag.T_plusOp) {
        parseTree.insert(new Node(Production.Intop))
        parseTree.insert(new Node(currentToken.attr))

        // Move the the current node back up the tree two levels.
        parseTree.current = parseTree.current.parent.parent
        // Prepare the next token for processing...
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

      var head = parseTree.insert(new Node(Production.StringExpr))

      if (isCurrentTokenValid(Tag.T_dblQuote))
        head.children.append(new Node(Terminal.DoubleQuote))
      else
        throw new ParseException("A String literal should begin with a double quote.")

      if (currentToken.tag == Tag.T_stringLiteral) {
        // Insert a node for the character list.
        parseTree.insert(new Node(Production.CharList))
        // Insert the string literal below the character list production.
        parseTree.insert(new Node(currentToken.attr))
        // Move the current pointer back up to the StringExpr node.
        parseTree.current = parseTree.current.parent.parent
        // Prepare the next token for processing.
        currentToken = tokenStream.dequeue
      } else
        throw new ParseException("A Expecting a string constant.")

      if (isCurrentTokenValid(Tag.T_dblQuote))
        head.children.append(new Node(Terminal.DoubleQuote))
      else
        throw new ParseException("A String should end with a double quote.")
    }

    /**
     * Parses a boolean expresion.
     *
     * BooleanExpr ::== ( Expr boolop Expr )
     *             ::== boolval
     */
    def booleanExpr() = {

      println("Parsing boolean expression...")

      var head = parseTree.insert(new Node(Production.BooleanExpr))

      // Determine whether this is a bool literal...
      if (currentToken.tag == Tag.T_boolLiteral) {
        // Add the production...
        parseTree.insert(new Node(Production.Boolval))
        // Add the bool literal...
        parseTree.insert(new Node(currentToken.attr))
        // Move the current pointer back up to the BooleanExpr node.
        parseTree.current = parseTree.current.parent.parent
        // Grab the next token for processing...
        currentToken = tokenStream.dequeue
      } else {
        // ...or a boolean expression.
        if (isCurrentTokenValid(Tag.T_openParen))
          head.children.append(new Node(Terminal.OpenParen))
        else
          throw new ParseException(
            "ERROR: A boolean expression begins with an open parenthesis.")

        expression()
        
        // Move the current pointer up to the parent.
        parseTree.current = parseTree.current.parent

        if (currentToken.tag == Tag.T_boolOp) {
          // Add the production...
          parseTree.insert(new Node(Production.Boolop))
          // Add the literal...
          parseTree.insert(new Node(currentToken.attr))
          // Move the current pointer back up to the BooleanExpr node.
          parseTree.current = parseTree.current.parent.parent
          // Grab the next token for processing...
          currentToken = tokenStream.dequeue
        } else {
          throw new ParseException(
            "ERROR: A boolean expression must contain a boolean operator.")
        }

        expression()
        // Move the current pointer up to the parent.
        parseTree.current = parseTree.current.parent
        
        if (isCurrentTokenValid(Tag.T_closeParen))
          head.children.append(new Node(Terminal.CloseParen))
        else
          throw new ParseException(
            "ERROR: A boolean expression ends with a closing parenthesis.")
        
        // Move the current pointer back up to the Expr node.
        parseTree.current = parseTree.current.parent
      }
    }

    def idExpr() {

      println("Parsing identifier expression...")

      // Verify that the current token is in fact an identifier.
      if (currentToken.tag == Tag.T_id)
        parseTree.insert(new Node(Production.Id))
      else
        throw new ParseException(
          "An indentifier should begin with an identifier.")

      // Add the character as a child node of the Id production...
      parseTree.insert(new Node(currentToken.attr))

      // Move the the current node back up the tree two levels.
      parseTree.current = parseTree.current.parent.parent

      // Prepare the next token for processing...
      currentToken = tokenStream.dequeue
    }

    def typeDecl() {

    }

    /**
     * Determines whether the current token matches the specified expected token.
     */
    def isCurrentTokenValid(expected: grammar.Tag.Value): Boolean = {

      println("Expecting " + Tag.getDescription(expected))
      println("Found " + Tag.getDescription(currentToken.tag))

      // Check whether the current token matches the expected token.
      var result: Boolean = currentToken.tag == expected

      // If so, grab the next token from the stream.
      if (result && !tokenStream.isEmpty) {
        currentToken = tokenStream.dequeue
      }

      // Return the result.
      result
    }

    // Kick-off parse
    program()

    while(!astNodes.isEmpty) {
      println(astNodes.dequeue.label.toString)
    }
    
    // Return the resulting CST.
    parseTree
  }
}
