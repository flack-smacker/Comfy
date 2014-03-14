/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

/**
 * Defines the productions that appear in our grammar.
 */
object Production extends Enumeration {
        
       type Tag = Value
       
       val Program, Block, StatementList, 
       Statement, PrintStatement, AssignmentStatement,
       WhileStatement, IfStatement, VarDecl,
       Expr, IntExpr, StringExpr, BooleanExpr,
       Id, CharList, Type, Char, Space, Digit,
       Boolop, Boolval, Intop = Value
}