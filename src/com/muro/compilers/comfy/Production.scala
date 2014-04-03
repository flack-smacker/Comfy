/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

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