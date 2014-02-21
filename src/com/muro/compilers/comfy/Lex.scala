/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

import scala.io._

import java.io._

object Lex {

  val tokens = new scala.collection.mutable.Queue[Token]
  val buffer = new scala.collection.mutable.ArrayBuffer[Char]
  
  val LeftBrace = '{'
  val RightBrace = '}'
  val LeftParen = '('
  val RightParen = ')'
  val DoubleQuote = '"'
  val EndOfProgram = '$'
  
  def tokenize(f: String): Unit = {
    Source.fromFile(new File(f)).getLines.foreach { line =>
      val charList: Array[Char] = line.toCharArray
      for (c <- charList) {
        println(c)
        c.isDigit
      }
    }
  }

  def consume(c: Char): Char = {
    if (c == RightParen) {
      c
    } else if (c.isLetter) {
      c
    } else if () {
      c
    } else if (c == LeftParen) {
      c
    }
  }

  def nextToken(): Token = {
    new Token(Tag._boolOp, "")
  }
  
  def hasNextToken(): Boolean = {
    !tokens.isEmpty
  }

  def main(args: Array[String]): Unit = {
    tokenize(args(0))
  }
  
}

