/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

/**
 * The Tag enumeration defines the set of valid lexemes recognized during
 * lexical analysis of a source program.
 */
object Tag extends Enumeration {

  type Tag = Value

  /**
   * The following syntax is shorthand for adding each name as a value to 
   * the Tag enumeration.
   */
  val T_if, T_while, T_print, T_boolOp, T_plusOp, T_assignOp, 
      T_openParen, T_closeParen, T_openBrace, T_closeBrace, 
      T_dblQuote, T_int, T_boolean, T_string, 
      T_id, T_numLiteral, T_boolLiteral, T_stringLiteral 
  = Value
}