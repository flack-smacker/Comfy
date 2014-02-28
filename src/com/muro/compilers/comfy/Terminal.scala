/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

object Terminal {
  val OpenBrace = "\\{"
  val CloseBrace = "\\}"
  val OpenParen = "\\("
  val CloseParen = "\\)"
  val DoubleQuote = "\""
  val Exclamation = "!"
  val Equals = "="
  val EndOfProgram = "$"
  val If = "if"
  val Print = "print"
  val While = "while"
  val Int = "int"
  val String = "string"
  val Boolean = "boolean"
  val EqualsOp = "=="
  val NotEqualsOp = "!="  
  val BoolLiteral= "(false)|(true)"
  val Plus = "\\+"
  val Space = " "
  val Newline = "\n"
  val Char = "[a-z]"
  val Id = Char
  val Operator = "(==)|(!=)|\\+"
  val Digit = "[0-9]{1}"
}
