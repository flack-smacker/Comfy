/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

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
    Lex.tokenize(filename)
    Lex.tokens.foreach( e => println(e.tag))
    Parse.parse(Lex.tokens)
  }
}
