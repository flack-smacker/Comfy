
package com.muro.compilers.comfy

import com.muro.compilers.comfy.exceptions.ParseException
import com.muro.compilers.comfy.grammar._

import com.muro.tree._

/**
 * Parse is responsible for building a parse tree from the specified sequence
 * of tokens.
 */
object Parse {

  /**
   * A symbol table for storing identifier type and scope information.
   */
  var symbols = SymbolTable

  /**
   * The concrete syntax tree resulting from parse.
   */
  var parseTree: Tree = new Tree()

  /**
   * The collection of nodes identified during parse for constructing the AST.
   */
  var astNodes = new scala.collection.mutable.Queue[Node]

  def parse(tokenStream: scala.collection.mutable.Queue[Token]) {

    /**
     * A reference to the token currently being parsed.
     */
    var currentToken = tokenStream.dequeue()

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

      // Status Output.
      println("Parsing block...")
      // As a result of this call, Block is the current node.
      var head = parseTree.insert(new Node(Production.Block))
      
      // Create a new environment...
      symbols.newEnv()
      // Status Output
      println("SYMBOL TABLE UPDATE: Initialized new block scope...")
      
      // Save this node for the AST.
      astNodes.enqueue(new Node("Block"))
      
      if (isCurrentTokenValid(Tag.T_openBrace))
        head.children.append(new Node(Terminal.OpenBrace))
      else
        throw new ParseException("A block should begin with an opening brace.")

      statementList()

      if (isCurrentTokenValid(Tag.T_closeBrace))
        head.children.append(new Node(Terminal.CloseBrace))
      else
        throw new ParseException("A block should end with an closing brace.")
      
      // We've exited a block so check if we need to adjust the scope.
      if (symbols.currentEnv != symbols.globalEnv)
        symbols.exitCurrentEnv()
      // Status Output
      println("SYMBOL TABLE UPDATE: Exiting current block scope...")
      
      // Save this node for the AST.
      astNodes.enqueue(new Node("EndBlock"))      
      
      // Leave the tree the way we found it.
      parseTree.current = parseTree.current.parent
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
      }

      // Leave the tree the way we found it.
      parseTree.current = parseTree.current.parent
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

      // Update the parse tree
      parseTree.insert(new Node(Production.Statement))
      // Status Output
      println("Parsing statement...")
      
      // Examine the next-token to determine which parse procedure to call.
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
        throw new ParseException("Invalid statement on line " + currentToken.line)

      parseTree.current = parseTree.current.parent
    }

    /**
     * Parses a print statement.
     *
     * PrintStatement ::== print ( Expr )
     */
    def printStatement() = {

      // Status output.
      println("Parsing print statement...")
      // Update the tree.
      var head = parseTree.insert(new Node(Production.PrintStatement))

      // Follow the grammar.
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

      // Save this node for the AST.
      astNodes.enqueue(head)
      // Leaev the tree the way we found it.
      parseTree.current = parseTree.current.parent
    }

    /**
     * Parses an assignment statement.
     *
     * AssignmentStatement ::== Id = Expr
     */
    def assignmentStatement() = {

      // Status output.
      println("Parsing assignment statement...")
      // Update the tree.
      var head = parseTree.insert(new Node(Production.AssignmentStatement))

      // We know that the current token is an identifier because this procedure
      // is invoked from statement() when the next token is a T_id. Therefore,
      // we can extract the identifier char so that we can update the symbol
      // table entry after successfully parsing the assignment.
      val id = currentToken.attr
      
      // We already know we have an identifier, but we invoke this procedure 
      // anyway because it scope checks the identifier and builds the Id tree.
      idExpr()

      // Check for an equals sign.
      if (isCurrentTokenValid(Tag.T_assignOp))
        head.children.append(new Node(Terminal.AssignmentOp))
      else
        throw new ParseException("Missing assignment operator on line" +
          currentToken.line)

      // Parse the right-hand side of the assignment.
      expression()

      // The assignment is valid (as far as scope) so update the symbol table.
      symbols.lookup(id).isDef = true
      // Status Output.
      println("SYMBOL TABLE UPDATE: Marking identifier '" + id + "' as defined.")

      // Save this node for construction of an AST.
      astNodes.enqueue(head)
      // Leave the tree the way we found it.
      parseTree.current = parseTree.current.parent
    }

    /**
     * Parses a variable declaration statement.
     *
     * VarDecl ::== type Id
     */
    def varDecl() = {

      // Status Output
      println("Parsing variable declaration statement...")
      // Update the tree
      parseTree.insert(new Node(Production.VarDecl))

      // Follow the grammar...
      if (currentToken.tag == Tag.T_type)
        parseTree.insert(new Node(Production.Type))
      else
        throw new ParseException(
          "A variable declaration should begin with a valid type.")

      // Extract the type specifier from the token.
      val t: String = currentToken.attr
      // Extract the line number from the token.
      val n: Integer = currentToken.line
      // Add the type specifier to the tree.
      parseTree.insert(new Node(t))
      // Move the current pointer back up to the VarDecl node.
      parseTree.current = parseTree.current.parent.parent

      // Grab the next token for processing...
      currentToken = tokenStream.dequeue

      // Follow the grammar...
      if (currentToken.tag == Tag.T_id)
        parseTree.insert(new Node(Production.Id))
      else
        throw new ParseException(
          "An indentifier should begin with an identifier??")

      // Extract the identifier from the token.
      val id: String = currentToken.attr
      // Check if this symbol has already been declared in the current scope.
      if (symbols.currentEnv.isDeclaredImmediate(id))
        throw new ParseException("Redeclaration found on line "  + n +
          ". Identifier " +  id + " already declared on line " + 
          symbols.lookup(id).line
          )
      else
        symbols.addSymbol(id, new Entry(t, n))
      // Status Output.
      println("SYMBOL TABLE UPDATE: Adding identifier '" + id + "' with type " + t)
      
      // Add the indentifier to the tree.
      parseTree.insert(new Node(id))
      // Move the the current node back up the tree two levels.
      parseTree.current = parseTree.current.parent.parent
      // Prepare the next token for processing...
      currentToken = tokenStream.dequeue

      // Save VarDecl nodes for the AST.
      astNodes.enqueue(parseTree.current)
      // Leave the tree the way we found it.
      parseTree.current = parseTree.current.parent
    }

    /**
     * Parses a while statement.
     *
     * WhileStatement ::== while BooleanExpr Block
     */
    def whileStatement() = {

      // Status output.
      println("Parsing while statement...")
      // Update the tree.
      var head = parseTree.insert(new Node(Production.WhileStatement))
      // Save this node for construction of the AST.
      astNodes.enqueue(head)
      
      // Follow the grammar.
      if (isCurrentTokenValid(Tag.T_while))
        head.children.append(new Node(Terminal.While))
      else
        throw new ParseException(
          "A while loop should begin with the \"while\" keyword.")

      // Parse the conditional expression
      booleanExpr()

      // Parse the body of the while loop
      block()

      // Leave the tree the way we found it...
      parseTree.current = parseTree.current.parent
    }

    /**
     * Parse an if statement.
     *
     * IfStatement ::== if BooleanExpr Block
     */
    def ifStatement() = {

      // Status output.
      println("Parsing if statement...")
      // Update the tree...
      var head = parseTree.insert(new Node(Production.IfStatement))
      // Save this node for construction of the AST.
      astNodes.enqueue(head)
      
      // Follow the grammar...
      if (isCurrentTokenValid(Tag.T_if))
        head.children.append(new Node(Terminal.If))
      else
        throw new ParseException(
          "An if statement should begin with the \"if\" keyword.")

      // Parse the conditional expression
      booleanExpr()

      // Parse the body of the if statement
      block()

      // Leave the tree the way we found it.
      parseTree.current = parseTree.current.parent
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

      // Status Output.
      println("Parsing expression...")
      // Update the tree.
      var head = parseTree.insert(new Node(Production.Expr))

      if (currentToken.tag == Tag.T_numLiteral)
        intExpr()
      else if (currentToken.tag == Tag.T_dblQuote)
        stringExpr()
      else if (currentToken.tag == Tag.T_openParen ||
        currentToken.tag == Tag.T_boolLiteral)
        booleanExpr()
      else if (currentToken.tag == Tag.T_id)
        idExpr()
      else
        throw new ParseException("Expecting an expression.")

      // Move the current pointer up to the parent.
      parseTree.current = parseTree.current.parent
    }

    /**
     * Parses an int expression.
     *
     * IntExpr ::== digit intop Expr
     */
    def intExpr() = {

      // Status output.
      println("Parsing integer expression...")
      // Update the tree.
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

        // Parse the right-hand side of the integer expression.
        expression()
      }

      // Leave the tree the way we found it.
      parseTree.current = parseTree.current.parent
    }

    /**
     * Parses a string expression.
     *
     * StringExpr ::== " CharList "
     */
    def stringExpr() = {

      // Status output.
      println("Parsing string expression...")
      // Update the tree.
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

      // Leave the tree the way we found it.
      parseTree.current = parseTree.current.parent
    }

    /**
     * Parses a boolean expresion.
     *
     * BooleanExpr ::== ( Expr boolop Expr )
     *             ::== boolval
     */
    def booleanExpr() = {

      // Status output.
      println("Parsing boolean expression...")
      // Update the tree.
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

        if (isCurrentTokenValid(Tag.T_closeParen))
          head.children.append(new Node(Terminal.CloseParen))
        else
          throw new ParseException(
            "ERROR: A boolean expression ends with a closing parenthesis.")
      }
      
      // Move the current pointer back up to the Expr node.
      parseTree.current = parseTree.current.parent
    }

    /**
     * Builds a node corresponding to an identifier using the global token
     * stream. The node is structured as follows:
     *        (Id)
     *          |
     *       (<char>)
     */
    def idExpr() {

      // Status Output
      println("Parsing identifier expression...")

      // Verify that the current token is in fact an identifier.
      if (currentToken.tag == Tag.T_id)
        parseTree.insert(new Node(Production.Id))
      else
        throw new ParseException(
          "An indentifier should begin with an identifier??")

      // Extract the identifier from the token.
      val id: String = currentToken.attr
      
      // Has this identifier been declared?
      if (!symbols.isDeclared(id))
        throw new ParseException("Undeclared Identifier " + id + " found on " +
          "line " + currentToken.line)
      
      // It has, so lookup its' symbol table entry.
      var entry = symbols.lookup(id)
      
      // Issue a warning if the variable has not been defined.
      // But look ahead in the token stream to determine if we are
      // currently parsing an assignment statement. We don't want to issue
      // the warning if we are in the process of assigning.
      if (!entry.isDef && (tokenStream.head.tag != Tag.T_assignOp))
        println("WARNING: Variable " + id + " on line " + currentToken.line + 
                " has not been defined. Using an undefined variable could " + 
                "result in unpredictable and possibly adventurous behavior.")
      // Increment the reference count.
      entry.refCount += 1
      
      // Add the indentifier to the tree.
      parseTree.insert(new Node(id))
      // Move the the current node back up the tree two levels.
      parseTree.current = parseTree.current.parent.parent
      // Prepare the next token for processing...
      currentToken = tokenStream.dequeue
    }

    /**
     * Determines whether the current token matches the specified expected token.
     */
    def isCurrentTokenValid(expected: grammar.Tag.Value): Boolean = {

      // Status Output.
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
  }
}
