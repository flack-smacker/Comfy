
package com.muro.compilers.comfy

/**
 * The Pattern class contains regular expressions describing each terminal
 * that appears in our grammar.
 */
object Pattern {
  val OpenBrace = "\\{".r
  val CloseBrace = "\\}".r
  val OpenParen = "\\(".r
  val CloseParen = "\\)".r
  val DoubleQuote = "\"".r
  val NotOperator = "!".r
  val AssignmentOp = "=".r
  val EndOfProgram = "\\$".r
  val If = "if".r
  val Print = "print".r
  val While = "while".r
  val Int = "int".r
  val String = "string".r
  val Boolean = "boolean".r
  val EqualsOp = "==".r
  val NotEqualsOp = "!=".r  
  val BoolLiteral = "false|true".r
  val Plus = "\\+".r
  val Space = " ".r
  val Newline = "\n".r
  val Char = "[a-z]".r
  val Id = Char
  val Operator = "(==)|(!=)|\\+".r
  val Digit = "[0-9]{1}".r
}
