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
    
    // = = = = = = = = = = = = = = =
    // PHASE 1 - Lexical Analysis
    // RESULT - Token Stream
    // = = = = = = = = = = = = = = = 
    println("Initiating lexical analysis on file \"" + filename + "\".")
    Lex.tokenize(filename)
    // Print the token stream.
    Lex.tokens.foreach( e => println("Found token " + e.tag + ":" + e.attr))
    println("Lexical analysis complete.")
    
    // = = = = = = = = = = = = = = =
    // PHASE 2 - Parse, Scope Check
    // RESULT - Concrete Syntax Tree, Symbol Table
    // = = = = = = = = = = = = = = =
    println("Parsing the resulting token stream.")
    Parse.parse(Lex.tokens)
    println("Parse complete.")
    // Print the CST
    println("Displaying the parse tree.")
    println(Parse.parseTree)
    // Print the symbol table
    println("Displaying the symbol table.")
    println(Parse.symbols)
    
    // = = = = = = = = = = = = = = =
    // PHASE 3 - Semantic Analysis, Type-Checking
    // RESULT - Abstract Syntax Tree
    // = = = = = = = = = = = = = = =
    
  }
}
