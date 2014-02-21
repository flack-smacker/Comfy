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
  val _if, _while, _print, _boolOp, _plusOp, _lParen, _rParen, _lBrace, _rBrace,
  _dblQuote, _int, _boolean, _string, _numLiteral, _boolLiteral, _stringLiteral 
  = Value
}