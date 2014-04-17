/*
 * An Env is a mapping from symbols to symbol table entries. An environment can itself contain environments (nested scope).
 */

package com.muro.compilers.comfy

class Env {

  /**
   * A mapping from symbols to symbol table entries for this environment.
   */
  private var mappings: java.util.Map[String, Entry] =
    new java.util.HashMap[String, Entry]()
  
  /**
   * A reference to this environments enclosing scope. The environment with
   * a null parent corresponds to global scope.
   */
  var parent: Env = null
  
  /**
   * A reference to the environments enclosed by this environment.
   */
   var children: scala.collection.mutable.ListBuffer[Env] = 
     new scala.collection.mutable.ListBuffer[Env]

  /**
   * Adds the specified symbol to this environment.
   */
  def addSymbol(sym: String, entry: Entry): Unit = {
    mappings.put(sym, entry)
  }
  
  /**
   * Adds the specified environment as a child to this environment. The specified
   * environemnt is "enclosed" by this environment. 
   */
  def addEnv(env: Env): Unit = {
    children.append(env)
  }
  
  /**
   * Returns the symbols defined in this environment. This method does NOT return
   * the symbols defined in the parent environments.
   */
  def getSymbols(): scala.collection.immutable.List[String] = {
    
    // Get all the symbols declared in this environment.
    var keys = mappings.keySet()
    
    // We need to convert this Set into a List.
    // 1. Initialize an array to hold the keys.
    var symbols: Array[String] = new Array[String](keys.size)
    // 2. Initialize the Array with the keys contained in the Set.
    symbols = keys.toArray(symbols)
    
    // Return the symbols in a list.
    symbols.toList
  }
  
  /**
   * Returns the symbol table entry mapped by the specified symbol. The entry
   * returned corresponds to the nearest enclosing scope.
   */
  def lookup(sym: String): Entry = {
    if (isDeclared(sym))
      doLookup(sym)
    else
      null
  }
  
  private def doLookup(sym: String): Entry = {
    
    // A dummy var to hold the current environment.
    var current: Env = this

    // A flag to indicate whether the symbol is defined.
    var entry: Entry = null

    // Loop until either the symbol is found or all scope has been checked.
    while (entry == null && current != null) {
      // Is sym declared in the current scope?
      entry = current.mappings.get(sym)
      // Move up to the enclosing scope.
      current = current.parent
    }
    
    entry
  }
  
  /**
   * Determines whether the specified symbol has been assigned a value
   * in this environment. In other words, this method returns true if the
   * specified symbol appears on the left-hand side of an assignment statement.
   */
  def isDefined(sym: String): Boolean = {
    mappings.get(sym).isDef || isDefinedInParent(sym)
  }

  /**
   * Determines whether the specified symbol is defined in one of this
   * environments enclosing environments.
   */
  private def isDefinedInParent(sym: String): Boolean = {

    // A dummy var to hold the current environment.
    var current: Env = parent

    // A flag to indicate whether the symbol is defined.
    var isDef: Boolean = false

    // Loop until either the symbol is found or all scope has been checked.
    while (current != null && !isDef) {
      // Is sym declared in the current scope?
      isDef = current.isDeclared(sym)
      // Move up to the enclosing scope.
      current = current.parent
    }

    isDef
  }

  /**
   * Determines whether the specified symbol is declared in this environment, or
   * in any environment enclosing this environment.
   */
  def isDeclared(sym: String): Boolean = {
    mappings.containsKey(sym) || isDeclaredInParent(sym)
  }
  
  /**
   * Determines whether the specified symbol is declared in the current environment.
   * This method does not check if the symbol is declared in an enclosing 
   * environment. To determine whether a symbol is declared in the immediate
   * environment or any enclosing environment use isDeclared.
   */
  def isDeclaredImmediate(sym: String): Boolean = {
    mappings.containsKey(sym)
  }

  /**
   * Determines whether the specified symbol is declared in one of this
   * environments enclosing environments.
   */
  private def isDeclaredInParent(sym: String): Boolean = {

    // A dummy var to hold the current environment.
    var current: Env = parent

    // A flag to indicate whether the symbol is defined.
    var isDecl: Boolean = false

    // Loop until either the symbol is found or all scope has been checked.
    while (current != null && !isDecl) {
      // Is sym declared in the current scope?
      isDecl = current.isDeclared(sym)
      // Move up to the enclosing scope.
      current = current.parent
    }

    isDecl
  }
  
  /**
   * Returns this environments enclosing scope.
   */
  def getEnclosingEnv(): Env = {
    return parent
  }
  
  /**
   * Returns a String representation of this environment.
   */
  override def toString(): String = {
    var toReturn = new StringBuilder()
    
    for(sym <- getSymbols()) {
      toReturn.append("ID:" + sym + " " + lookup(sym) + "\n")
    }
    
    if(toReturn.isEmpty)
      toReturn.append("EMPTY ENV\n")
    // Return the resulting String.
    toReturn.toString
  }
}

class Entry(var idType: String, var line: Integer, 
            var isDef: Boolean = false, var refCount: Integer = 0) {
  override def toString(): String = {
    var warn = ""
    // Issue a warning if this identifier was unused.
    if (refCount == 0)
      warn = " -> WARNING: Identifier declared but unused."
    
    "TYPE:" + idType + 
    " DECLINE:" + line + 
    " ISDEFINED:" + isDef + 
    " REFCOUNT:" + refCount +
    warn
  }
}
