/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

import Tag._

class Token(t: Tag, a: String) {
    var tag: Tag = t
    var attr: String = a
    
    /**
     * Returns a string describing this token.
     */
    def getDescription():String = {
      tag match {
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
        case Tag.T_id => "identifier"
        case Tag.T_type => "type declaration"
        case Tag.T_boolLiteral => "boolean literal"
        case Tag.T_numLiteral => "numeric literal"
        case Tag.T_stringLiteral => "string literal"
        case Tag.T_endOfProgram => "end-of-program marker"
        case _ => "Expecting nothing"
      }
    }
}
