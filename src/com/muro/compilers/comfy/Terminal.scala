/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

object Terminal {
  val OpenBrace = "\\{".r
  val CloseBrace = "\\}".r
  val OpenParen = "\\(".r
  val CloseParen = "\\)".r
  val DoubleQuote = "\"".r
  val Exclamation = "!".r
  val Equals = "=".r
  val EndOfProgram = "\\$".r
  val If = "if".r
  val Print = "print".r
  val While = "while".r
  val Int = "int".r
  val String = "string".r
  val Boolean = "boolean".r
  val EqualsOp = "==".r
  val NotEqualsOp = "!=".r  
  val BoolLiteral= "(false)|(true)".r
  val Plus = "\\+".r
  val Space = " ".r
  val Newline = "\n".r
  val Char = "[a-z]".r
  val Id = Char
  val Operator = "(==)|(!=)|\\+".r
  val Digit = "[0-9]{1}".r
}
