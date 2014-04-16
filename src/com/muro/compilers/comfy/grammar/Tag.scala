/*
 * 
 */

package com.muro.compilers.comfy.grammar

/**
 * The Tag enumeration defines the elements of our grammar. Tags are used
 * during lexical analysis to name each Token.
 */
object Tag extends Enumeration {

  type Tag = Value

  /**
   * The following syntax is shorthand for adding each name as a value to 
   * the Tag enumeration.
   */
  val T_if, T_while, T_print, T_boolOp, T_plusOp, T_assignOp, 
      T_openParen, T_closeParen, T_openBrace, T_closeBrace, 
      T_dblQuote, T_type, T_int, T_boolean, T_string, T_endOfProgram,
      T_id, T_numLiteral, T_boolLiteral, T_stringLiteral
  = Value
  
  def getDescription(toDescribe: Tag.Value):String = {
      toDescribe match {
        case Tag.T_assignOp => "assignment operator"
        case Tag.T_boolOp => "boolean operator"
        case Tag.T_plusOp => "addition operator"
        case Tag.T_if => "keyword \"if\""
        case Tag.T_while => "keyword \"while\""
        case Tag.T_print => "keyword \"print\""
        case Tag.T_openParen => "open parenthesis."
        case Tag.T_closeParen => "close parenthesis"
        case Tag.T_openBrace => "open brace"
        case Tag.T_closeBrace => "close brace"
        case Tag.T_dblQuote => "double quote"
        case Tag.T_id => "identifier"
        case Tag.T_type => "type declaration"
        case Tag.T_boolLiteral => "boolean literal"
        case Tag.T_numLiteral => "numeric literal"
        case Tag.T_stringLiteral => "string literal"
        case Tag.T_endOfProgram => "end-of-program marker"
      }
  }
}