/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy.grammar

/**
 * Tokens represent elements in the source code that correspond to lexemes 
 * contained in the grammar. Each token contains a Tag, which is is an 
 * enumeration type describing the token's type (e.g.: assignOp, boolOp). Each 
 * token can also store  an optional attribute. An example would be a Token 
 * representing a boolean operation. The tag value would be "T_boolOp" and the 
 * attribute would be either "!=" or "==".
 */
class Token(val tag: Tag.Value, val line: Integer, val attr: String = "")
