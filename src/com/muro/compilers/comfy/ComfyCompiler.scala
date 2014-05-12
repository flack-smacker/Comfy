/*
 * 
 */

package com.muro.compilers.comfy

import com.muro.tree.Tree

object ComfyCompiler {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    if (args.length < 1)
      println("Please provide a filename.")
    else {
      compile(args(0))
    }
  }
  
  def compile(filename :String) = {
    
    try {    
    // = = = = = = = = = = = = = = =
    // PHASE 1 - Lexical Analysis
    // RESULT - Token Stream
    // = = = = = = = = = = = = = = = 
    println("Initiating lexical analysis on file \"" + filename + "\".")
    Lex.tokenize(filename)
    println("Lexical analysis complete.")
    
    // Print the token stream.
    println("========== Displaying the Token Stream ============")
    Lex.tokens.foreach( e => println("Found token " + e.tag + ":" + e.attr))
    println("==========================================")
    
    // = = = = = = = = = = = = = = =
    // PHASE 2 - Parse, Scope Check
    // RESULT - Concrete Syntax Tree, Symbol Table
    // = = = = = = = = = = = = = = =
    println("Parsing the resulting token stream.")
    Parse.parse(Lex.tokens)
    println("Parse complete.")
    
    // Print the CST
    println("========== Displaying the CST ============")
    println(Parse.parseTree)
    println("==========================================")
    
    // Print the symbol table
    println("===== Displaying the Symbol Table ========")
    println(Parse.symbols)
    println("==========================================")
    
    // = = = = = = = = = = = = = = =
    // PHASE 3 - Semantic Analysis, Type-Checking
    // RESULT - Abstract Syntax Tree
    // = = = = = = = = = = = = = = =
    println("Constructing the AST")
    var ast: Tree = Analyzer.constructAST(Parse.astNodes)
    println("Construction complete.")
    
    // Output the AST.
    println("========== Displaying the AST ============")
    println(ast)
    println("==========================================")
    
    // = = = = = = = = = = = = = = =
    // PHASE 4 - Code Generation
    // RESULT - 6502a Instructions
    // = = = = = = = = = = = = = = =
    println("Generating Code")
    val opcodes = CodeGenerator.generate(ast)
    println("Code Generation Complete")
    
    // Output the instruction stream.
    println(opcodes.toString)
    
    } catch {
      case e: Exception => println("ERROR: " + e.getMessage)
    }
  }
}
