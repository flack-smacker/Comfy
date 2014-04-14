/*
 *
 */

package com.muro.compilers.comfy.grammar

/**
 * Defines the elements that appear in the parse tree.
 */
object Production extends Enumeration {
        
       type Tag = Value
       
       val Program, Block, StatementList, 
       Statement, PrintStatement, AssignmentStatement,
       WhileStatement, IfStatement, VarDecl,
       Expr, IntExpr, StringExpr, BooleanExpr, IdExpr,
       Id, CharList, Type, Char, Space, Digit,
       Boolop, Boolval, Intop, Terminal = Value
}