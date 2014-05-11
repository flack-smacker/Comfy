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
  private val opstream: StringBuilder = new StringBuilder()

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
  val TEMP_HEAP_ADDRESS = "FF 00"

  def generate(ast: Tree): String = {

    // Kick-off code generation at the root.
    doGen(ast.root)
    
    // Insert a break to end the program.
    opstream.append(Opcodes.BRK)
    
    // Convert the opstream string into an array.
    var ops = opstream.toString.split(" ")
    
    // Calculate the size (in bytes) of the generated code.
    val codeSizeBytes = ops.length
    
    // We now know where the static data area begins. So we backpatch.
    backpatch(codeSizeBytes + 1, opstream.toString)
  }
  
  def backpatch(beginAddr: Int, img: String):String = {
    
    // This will store the executable image after backpatching. 
    var toReturn = img
    
    // An iterator will allow us to iterate over the dummy var entries.
    val iter = staticData.keySet.iterator
    
    while (iter.hasNext) {
      // Get the placeholder value that needs to be patched.
      var tempName = iter.next
      
      // Calculate the address in the static data area.
      var virtAddr = (staticData.get(tempName).offset + beginAddr).toHexString
      
      // Perform the replacement.
      toReturn = toReturn.replaceAll(tempName, virtAddr)
    }
    
    toReturn.replaceAllLiterally("XX", "00")
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
        toExpand.children(0).label,
        0,
        false,
        0,
        tempName))

    // Add the required machine instructions to the opstream.
    // Initialize the accumulator to 0.
    opstream.append(Opcodes.LDA_C + " 00 ")
    // Generate the store instruction, inserting the placeholder variable.
    opstream.append(Opcodes.STA + " " + tempName + " XX ")

    // Increment the count of static variables
    staticVarCount += 1
  }

  def expandAssignment(toExpand: Node) {
    // Evaluate the expression on the right-hand side.
    expandExp(toExpand.children(1))
    // Load the value from the temporary memory location.
    opstream.append(Opcodes.LDA_M + " " + TEMP_HEAP_ADDRESS + " ")
    
    // Extract the variable name from the node.
    val varName: String = toExpand.children(0).label
    // Lookup the dummy value for this variable.
    val toAssign: String = SymbolTable.lookup(varName).tempName
    // Store the value in the location assigned to this variable.
    opstream.append(Opcodes.STA + " " + toAssign + " XX ")
  }

  def expandIf(cond: Node) {

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

    // Determine whether this is a string variable.
    if (exprNode.label.matches(Pattern.Id.toString)) {
      val varType = SymbolTable.lookup(exprNode.label).idType
      if (varType.equals(ComfyType.String)) {
        // Do some String related print stuff.
        return
      }
    }

    // Generate the instructions necessary for evaluating the expression.
    expandExp(exprNode)
    // Load the Y register with the value to be printed.
    opstream.append(Opcodes.LDY_M + " " + TEMP_HEAP_ADDRESS + " ")
    // Prime the X register for a print system call.
    opstream.append(Opcodes.LDX_C + " 01 ")
    // Execute the print by issuing a system call.
    opstream.append(Opcodes.SYS + " ")
  }

  def expandExp(expr: Node) {

    // Extract the node label for this expression.
    val value = expr.label

    // Determine the type of expression.
    value match {

      case Pattern.BoolLiteral() => {
        // Load the literal into the accumulator.
        if (value.equals(BoolTrue))
          opstream.append(Opcodes.LDA_C + " 01")
        else if (value.equals(BoolFalse))
          opstream.append(Opcodes.LDA_C + " 00")
        // Store it into the heap.
        opstream.append(Opcodes.STA + " " + TEMP_HEAP_ADDRESS + " ")
      }

      case Pattern.IntLiteral() => {
        // Load the literal into the accumulator
        opstream.append(Opcodes.LDA_C + " 0" + value + " ")
        // Store it into the heap.
        opstream.append(Opcodes.STA + " " + TEMP_HEAP_ADDRESS + " ")
      }
        
      case Pattern.StringLiteral() => {
            // initialize heap with string literal
      }
        
      case Pattern.Id() => {
          // Look-up the placeholder value for this identifier.
          val entry = SymbolTable.lookup(value)
          // Load the value of the variable into the accumulator.
          opstream.append(Opcodes.LDA_M + " " + entry.tempName + " XX ")
          // Store it into the heap.
          opstream.append(Opcodes.STA + " " + TEMP_HEAP_ADDRESS + " ")
      }
        
      case "+" => {
          
        // Evaluate the right-hand side of the expression.
        expandExp(expr.children(1))
        
        // Extract the integer literal contained on the left-hand side.
        val intLit = expr.children(0).label
        
        // Store the integer literal in the accumulator.
        opstream.append(Opcodes.LDA_C + " 0" + intLit + " ")
        
        // Add the result of the right-hand side of the expression to the acc.
        opstream.append(Opcodes.ADC + " " + TEMP_HEAP_ADDRESS + " ")
        
        // Store the result in the heap.
        opstream.append(Opcodes.STA + " " + TEMP_HEAP_ADDRESS + " ")
      }
    }
  }

  def expandIntExp(toExpand: Node) {

  }

  def expandBoolExp(toExpand: Node) {

  }

  def expandStringExp(toExpand: Node) {

  }

  class StaticEntry(var idName: String,
                    var idType: String,
                    var offset: Integer) {

    override def toString(): String = {
      idName + offset + idType
    }
  }
}