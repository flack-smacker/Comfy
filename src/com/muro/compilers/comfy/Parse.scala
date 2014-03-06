/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

object Parse {
  
  def parse(tokenStream: scala.collection.mutable.Queue[Token]) = {
    
    var currentToken = tokenStream.dequeue()
    
    def program() = {
      block()
    }
    
    def block() = {
      
      if (currentToken == Tag.T_openBrace)
        println("ERROR: A block should begin with an opening brace.")
      
      statementList()
      
      if (currentToken == Tag.T_closeBrace)
        println("ERROR: A block should end with an opening brace.")
    }
    
    def statementList() = {
      
    }
    
    def statement() = {
      
    }
    
    def printStatement() = {
      
    }
    
    def assignmentStatement() = {
      
    }
    
    def varDecl() = {
      
    }
    
    def whileStatement() = {
      
    }
    
    def ifStatement() = {
      
    }
    
    def expression() = {
      
    }
    
    def intExpr() = {
      
    }
    
    def stringExpr() = {
      
    }
    
    def booleanExpr() = {
      
    }
    
    def anIdentifier() = {
      
    }
    
    def charList() = {
      
    }
    
    def aType() = {
      
    }
    
    def aChar() = {
      
    }
    
    def aSpace() = {
      
    }
    
    def aDigit() = {
      
    }
    
    def boolOperation() = {
      
    }
    
    def boolValue() = {
      
    }
    
    def intOperation() = {
      
    }
    
    def verifyToken(t: Token): Unit = {
      
    }
  }
}
