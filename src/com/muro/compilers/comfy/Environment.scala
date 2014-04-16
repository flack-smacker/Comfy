/*
 * 
 */

package com.muro.compilers.comfy

class Env {
  private val mappings: java.util.Map[String, Entry] = 
    new java.util.HashMap[String, Entry]()
  
    def isDefined(identifier: String): Boolean = {
      true
    }
}
