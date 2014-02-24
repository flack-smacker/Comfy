/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

import scala.io._
import java.io._

object Lex {

  /**
   * Contains the tokens extracted from the source input.
   */
  val tokens = new scala.collection.mutable.Queue[Token]
  
  /**
   * Contains the characters awaiting lexical analysis.
   */
  val buffer = new scala.collection.mutable.ArrayBuffer[Char]
  
  var nLetters: Int = 0
  var nSpaces: Int = 0
  var nNewlines: Int = 0
  var nPunctuation: Int = 0
  var nDigits = 0
  var nUnclassified = 0
  
  def tokenize(f: String): Unit = {
    
    val source: StringBuilder = new StringBuilder()
    
    Source.fromFile(new File(f)).getLines.foreach { line =>
      // Strip leading and trailing whitespace and reduce extraneous whitespace.
      source.append(line.trim().replaceAll("(\\s)(2,}", " ") + "\n")
    }
    
    val i: Int = 0
    
    if (source.charAt(0) != '{') {
      // Insert a left brace and issue a warning.
    }
    
    if (source.charAt(source.length-1) != '$') {
      // Insert the end-of-program delimiter and issue a warning.
    }
    
    Source.fromIterable(source).foreach { c =>
      consume(c)
    }
    
    def consume(c: Char): Unit = {
    if (c.isDigit) 
      nDigits += 1
    else if (c.isLetter)
      nLetters += 1
    else if (c.isSpaceChar)
      nSpaces += 1
    else if (c =='\n')
      nNewlines += 1
    else
      System.out.print(" " + c)
  }
    
    System.out.println("\nLetters: " + nLetters)
    System.out.println("Digits: " + nDigits)
    System.out.println("Newlines: " + nNewlines)
    System.out.println("Spaces: " + nSpaces)
    System.out.println("Unclassified: " + nUnclassified)
  }

  def nextToken(): Token = {  
    if (hasNextToken())
      tokens.dequeue
    else
      null
  }
  
  def hasNextToken(): Boolean = {
    !tokens.isEmpty
  }
  
  def main(args: Array[String]): Unit = {
    tokenize(args(0))
  }
}