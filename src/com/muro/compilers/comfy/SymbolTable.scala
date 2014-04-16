/*
 * 
 */

package com.muro.compilers.comfy

class SymbolTable(val envs: java.util.List[Env]) {
  
  var currentEnv = null;
  
  
}

class Entry(var idType: String, var isDefined: Boolean = false)
