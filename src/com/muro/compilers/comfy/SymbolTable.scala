/**
 *
 */

package com.muro.compilers.comfy

object SymbolTable {

  /**
   * A reference to the global environment. The global environment encloses
   * all environments. It can be used to traverse the environment tree.
   */
  var globalEnv: Env = null

  /**
   * A reference to the current environment.
   */
  var currentEnv: Env = globalEnv

  /**
   * Maps the specified symbol to the specified entry and adds it to the
   * current environment.
   */
  def addSymbol(sym: String, entry: Entry): Unit = {
    currentEnv.addSymbol(sym, entry)
  }
 
  /**
   * Returns the symbol table entry for the specified symbol.
   */
  def lookup(sym: String): Entry = {
     currentEnv.lookup(sym)
  }

  /**
   * Creates a new empty environment enclosed by the current environment. This
   * method updates the current environment to the newly created environment.
   */
  def newEnv(): Env = {
    // Create a new empty environment.
    val env = new Env()
    
    // Has the top-level environment been initialized yet?
    if (globalEnv == null)
      globalEnv = env
    else
      currentEnv.addEnv(env)
    
    // Initialize the new environments parent to the current environment.
    env.parent = currentEnv
    // Make the new environment the current environemnt
    currentEnv = env
    // Return a reference to the new env
    env
  }

  /**
   * Updates the current environment pointer to the next enclosing scope.
   */
  def exitCurrentEnv() {
    currentEnv = currentEnv.getEnclosingEnv();
  }

  /**
   * Determines whether the specified symbol has been declared in the current
   * environment or any enclosing environments.
   */
  def isDeclared(sym: String): Boolean = {
    currentEnv.isDeclared(sym)
  }

  /**
   * Determines whether the specified symbol has been defined in the current
   * environment or any enclosing environments.
   */
  def isDefined(sym: String): Boolean = {
    currentEnv.isDefined(sym)
  }
 
  override def toString(): String = {
    // Start with an empty StringBuilder
    val toReturn: StringBuilder = new StringBuilder()
    // Kick-off the recursive method for building a String representation of the env.
    buildEnvString(this.globalEnv, 0, toReturn)
    // Return the result.
    toReturn.toString
  }
 
  private def buildEnvString(env: Env, nestingLvl: Integer, envStr: StringBuilder) {
    // Get the symbols defined in this env.
    val syms: List[String] = env.getSymbols
     
    // Append the header for this environment.
    envStr.append("Nesting Level: " + nestingLvl + "\n")
    
    // Append the String representation of this environment.
    envStr.append(env.toString)
    
    // Append the children of this environment.
    for (e <- env.children) {
      buildEnvString(e, nestingLvl+1, envStr)
    }
  }
}

