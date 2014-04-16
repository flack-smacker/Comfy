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
    
    println("Initiating lexical analysis on file \"" + filename + "\".")
    
    // = = = = = = = = = = = = = = =
    // PHASE 1 - Lexical Analysis
    // RESULT - Token Stream
    // = = = = = = = = = = = = = = = 
    Lex.tokenize(filename)
    
    // Print the results
    Lex.tokens.foreach( e => println("Found token " + e.tag + ":" + e.attr))
    println("Lexical analysis complete. Beginning parse...")
    
    // = = = = = = = = = = = = = = =
    // PHASE 2 - Parse
    // RESULT - Concrete Syntax Tree
    // = = = = = = = = = = = = = = =
    val cst:Tree = Parse.parse(Lex.tokens)
    // Print the CST
    println(cst.toString)
    
    // = = = = = = = = = = = = = = =
    // PHASE 3 - Semantic Analysis
    // RESULT - Abstract Syntax Tree
    // = = = = = = = = = = = = = = =
    //val ast: Tree = Analyzer.constructAST(cst)
    //println(ast.toString)
  }
}
