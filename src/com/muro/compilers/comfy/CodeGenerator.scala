/*
 *
 */

package com.muro.compilers.comfy

import com.muro.tree.Node
import com.muro.tree.Tree
import com.muro.compilers.comfy.grammar.Pattern
import com.muro.compilers.comfy.grammar.Terminal._

object CodeGenerator {

  /**
   * The sequence of 6502a opcodes generated from the specified AST.
   */
  private val opstream = new scala.collection.mutable.ArrayBuffer[String](256)
  
  /**
   * A storage area for heap data.
   */
  private val heap = new scala.collection.mutable.ArrayBuffer[String](100)
  
  /**
   * A mapping from temp names to static entries.
   */
  private val staticData: java.util.Map[String, StaticEntry] =
    new java.util.HashMap[String, StaticEntry]()

  /**
   * This will track the number of variables in the static area. This value is
   * used when generating code for variable declaration statements to name
   * temporary values and as the offset into the static data area.
   */
  private var staticVarCount: Integer = 0
  
  /**
   * The prefix used when creating temporary variables in the static table.
   */
  val TEMP_PREFIX: String = "T"

  /**
   * A location in the heap used for storing temporary values.
   */
  val TEMP_HEAP_ADDRESS = "FF"
  
  /**
   * A constant to reprsent a byte value of 0 in HEX.
   */
  val ZERO_BYTE_HEX = "00"
  
  val BOOL_TRUE = "01"
  
  val BOOL_FALSE = "00"

  def generate(ast: Tree): String = {

    // Kick-off code generation at the root.
    doGen(ast.root)
    
    // Insert a break to end the program.
    opstream.append(Opcodes.BRK)
    
    // Calculate the size (in bytes) of the generated code.
    val codeSizeBytes = opstream.size
    
    // We now know where the static data area begins. So we backpatch.
    backpatch(codeSizeBytes + 1)
    
    // We can also calculate the start of the heap data area.
    val heapStart = (codeSizeBytes + 1) + staticVarCount
    
    // Convert the array into a string.
    var bin = new StringBuilder(255)
    
    opstream.foreach { byte => bin.append(byte + " ") }
    
    bin.toString
  }
  
  def backpatch(beginAddr: Int) {
    
    // An iterator will allow us to iterate over the dummy var entries.
    val iter = staticData.keySet.iterator
    
    while (iter.hasNext) {
      // Get the placeholder value that needs to be patched.
      var tempName = iter.next
      
      // Calculate the address in the static data area.
      var virtAddr = (staticData.get(tempName).offset + beginAddr).toHexString
      
      // Perform the replacement.
      for (i <- 0 to opstream.length - 1) {
        if (opstream(i).equals(tempName))
          opstream(i) = virtAddr
      }
    }
  }

  private def doGen(node: Node) {

    node.label match {

      case "Block" => {
        expandBlock(node)
      }

      case "VarDecl" => {
        expandVarDecl(node)
      }

      case "Assignment" => {
        expandAssignment(node)
      }

      case "If" => {
        expandIf(node)
      }

      case "While" => {
        expandWhile(node)
      }

      case "Print" => {
        expandPrint(node)
    }
            
        case _ => {
            // do nothing?
        }
      }
  }

  def expandBlock(toExpand: Node) {
    // We are entering a new block, so increase the nesting level.
    SymbolTable.newEnv()

    // Generate code for each statement in this block.
    toExpand.children.foreach((node: Node) => doGen(node))

    // We are exiting the current block, do decrease the nesting level.
    SymbolTable.exitCurrentEnv()
  }

  def expandVarDecl(toExpand: Node) {
    // Construct the name of the temporary variable.
    var tempName = TEMP_PREFIX + staticVarCount

    // Construct the entry for the static table.
    var entry = new StaticEntry(
      toExpand.children(1).label, // var name
      toExpand.children(0).label, // type
      staticVarCount // offset into static data area
      )
    // Store the entry in the static table
    staticData.put(tempName, entry)

    // Add the identifier to our symbol table.
    SymbolTable.addSymbol(toExpand.children(1).label,
      new com.muro.compilers.comfy.Entry(
        toExpand.children(0).label, // type
        0, // reference count
        false, // is defined
        0, // line number
        tempName)) // dummy variable name

    // Initialize the accumulator to 0.
    opstream.append(Opcodes.LDA_C, ZERO_BYTE_HEX)
    // Generate the store instruction, inserting the placeholder variable.
    opstream.append(Opcodes.STA, tempName, ZERO_BYTE_HEX)
    
    // Increment the count of static variables
    staticVarCount += 1
  }

  def expandAssignment(toExpand: Node) {
    // Evaluate the expression on the right-hand side.
    expandExp(toExpand.children(1))
    // Load the resulting value from the temporary memory location.
    opstream.append(Opcodes.LDA_M, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
    
    // Extract the variable name from the node.
    val varName = toExpand.children(0).label
    // Lookup the dummy value for this variable.
    val toAssign = SymbolTable.lookup(varName).tempName
    // Store the value in the location assigned to this variable.
    opstream.append(Opcodes.STA, toAssign, ZERO_BYTE_HEX)
  }

  def expandIf(ifNode: Node) {
    
    // Evaluate the conditional expression
    expandExp(ifNode.children(0))
    
    // Load the resulting value into the X register.
    opstream.append(Opcodes.LDX_M, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
    
    // Load and store a true value into the heap.
    opstream.append(Opcodes.LDA_C, BOOL_TRUE)
    opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
    
    // Perform a comparison. If the Z flag is not set then we know that the
    // boolean expression resulted in a false value.
    opstream.append(Opcodes.CPX, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
    
    // Insert the branch opcode.
    opstream.append(Opcodes.BNE)
    
    // This is the index where we will insert the number of bytes to jump.
    val jmpIndex = opstream.size
    
    // Evaluate the conditional block.
    expandBlock(ifNode.children(1))
    
    // Calculate the length of the block.
    val blockLengthBytes = (opstream.size - jmpIndex)
    
    // Left pad the jump address with zeros.
    var jmpAddr = blockLengthBytes.toHexString
    
    if (jmpAddr.length == 1)
      jmpAddr = "0" + jmpAddr
    
    // Insert the jump amount.
    opstream.insert(jmpIndex, jmpAddr)
  }

  def expandWhile(loop: Node) {

  }

  def expandPrint(node: Node) {

    // Extract the expression node to be printed.
    val exprNode = node.children(0)

    // Determine whether this is a string literal.
    if (exprNode.label.matches(Pattern.StringLiteral.toString)) {
      // Do some String related print stuff.
      return
    }

    // Generate the instructions necessary for evaluating the expression.
    expandExp(exprNode)
    
    // Load the Y register with the value to be printed.
    opstream.append(Opcodes.LDY_M, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
    
    // Determine the type of value to be printed.
    if (exprNode.label.matches(Pattern.Id.toString)) {
      val varType = SymbolTable.lookup(exprNode.label).idType
      // Prime the X register for a print system call.
      if (varType.equals(ComfyType.String)) {
        // Print a string.
        opstream.append(Opcodes.LDX_C, "02")
      } else {
        // Print an integer.
        opstream.append(Opcodes.LDX_C, "01")
      }
    }
    
    // Execute the print by issuing a system call.
    opstream.append(Opcodes.SYS)
  }

  def expandExp(expr: Node) {

    // Extract the node label for this expression.
    val value = expr.label

    // Determine the type of expression.
    value match {

      case Pattern.BoolLiteral() => {
        // Load the literal into the accumulator.
        if (value.equals(BoolTrue))
          opstream.append(Opcodes.LDA_C, BOOL_TRUE)
        else if (value.equals(BoolFalse))
          opstream.append(Opcodes.LDA_C, BOOL_FALSE)
        
        // Store it into the heap.
        opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
      }

      case Pattern.IntLiteral() => {
        // Load the literal into the accumulator
        opstream.append(Opcodes.LDA_C, "0" + value)
        // Store it into the heap.
        opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
      }
        
      case Pattern.StringLiteral() => {
        // initialize heap with string literal
      }
        
      case Pattern.Id() => {
          // Look-up the placeholder value for this identifier.
          val entry = SymbolTable.lookup(value)
          // Load the value of the variable into the accumulator.
          opstream.append(Opcodes.LDA_M, entry.tempName, ZERO_BYTE_HEX)
          // Store it into the heap.
          opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
      }
        
      case "+" => {
          
        // Evaluate the right-hand side of the expression.
        expandExp(expr.children(1))
        
        // Extract the integer literal contained on the left-hand side.
        val intLit = expr.children(0).label
        
        // Store the integer literal in the accumulator.
        opstream.append(Opcodes.LDA_C, "0" + intLit)
        
        // Add the result of the right-hand side of the expression to the acc.
        opstream.append(Opcodes.ADC, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
        
        // Store the result in the heap.
        opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
      }
      
      case "==" => {
        // Evaluate the left-hand side of the expression.
        expandExp(expr.children(0))
        
        // Load the resulting value into the X register.
        opstream.append(Opcodes.LDX_M, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
        
        // Evaluate the right-hand side of the expression. The resulting
        // value is stored in the heap.
        expandExp(expr.children(1))
        
        // Perform the comparison.
        opstream.append(Opcodes.CPX, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
        
        /**
         * We need to store the result of the comparsion in the heap. 
         * This is accomplished through a series of branch and compare instructions.
         * The first branch will skip twelve bytes to load a value of false 
         * into the heap.         
         */
        opstream.append(Opcodes.BNE, "0C")
          
        // If the values were equal the branch is not executed and we load a 
        // boolean true into the heap. 
        opstream.append(Opcodes.LDA_C, BOOL_TRUE)
        opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
        
        // If the above instructions are executed we need to skip the instructions 
        // for loading a true vaule into the heap. If the comparison resulted in
        // a false then we know that the heap address contains a boolean false.
        // Therefore, we can force another branch by comparing the false to a true.
        opstream.append(Opcodes.LDX_C, BOOL_FALSE)
        opstream.append(Opcodes.CPX, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
        opstream.append(Opcodes.BNE, "05")
          
        // If the values were not equal we branch here and load a boolean false
        // value into the heap.
        opstream.append(Opcodes.LDA_C, BOOL_FALSE)
        opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
      }
      
      case "!=" => {
        // Evaluate the left-hand side of the expression.
        expandExp(expr.children(0))
        
        // Load the result of the evaluation into the X register.
        opstream.append(Opcodes.LDX_M, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
        
        // Evaluate the right-hand side of the expression. The resulting
        // value is stored in the heap.
        expandExp(expr.children(1))
        
        // Perform the comparison.
        opstream.append(Opcodes.CPX, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)  
        
        /**
         * Insert the branch instructions. This will determine whether we load
         * a true value into the heap, or a false value. The process of loading
         * either value requires 2 instructions which result in five bytes. The
         * branch allows us to execute the instructions for loading either a
         * true or false.
         */
        opstream.append(Opcodes.BNE, "0C")
        
        // We are checking for NOT EQUALS. Therefore, if the values were equal 
        // the branch is not executed and a false value is stored in the heap.
        opstream.append(Opcodes.LDA_C, BOOL_FALSE)
        opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
        
        // If the above instructions are executed we need to skip the instructions 
        // for loading a true vaule into the heap. If the comparison resulted in
        // a false then we know that the heap address contains a boolean false.
        // Therefore, we can force another branch by comparing the false to a true.
        opstream.append(Opcodes.LDX_C, BOOL_TRUE)
        opstream.append(Opcodes.CPX, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
        opstream.append(Opcodes.BNE, "05")
        
        // If the values were not equal the branch is executed and we load a 
        // boolean true into the heap.
        opstream.append(Opcodes.LDA_C, BOOL_TRUE)
        opstream.append(Opcodes.STA, TEMP_HEAP_ADDRESS, ZERO_BYTE_HEX)
      }
    }
  }
  
  class StaticEntry(var idName: String,
                    var idType: String,
                    var offset: Integer) {

    override def toString(): String = {
      idName + offset + idType
    }
  }
}