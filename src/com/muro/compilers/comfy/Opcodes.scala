/*
 * This class contains a set of named constants for each 6502 opcode supported
 * by our compiler.
 */

package com.muro.compilers.comfy

object Opcodes {
  
  /**
   * Load the accumulator with a constant.
   */
  val LDA_C: String = "A9"
  
  /**
   * Load the accumulator from memory.
   */
  val LDA_M: String = "AD"
  
  /**
   * Store the accumulator in memory.
   */
  val STA: String = "8D"
  
  /**
   * Add with carry.
   */
  val ADC: String = "6D"
  
  /**
   * Load the X register with a constant.
   */
  val LDX_C: String = "A2"
  
  /**
   * Load the X register from memory.
   */
  val LDX_M: String = "AE"
  
  /**
   * Load the Y register with a constant.
   */
  val LDY_C: String = "A0"
  
  /**
   * Load the Y register from memory.
   */
  val LDY_M: String = "AC"
  
  /**
   * No Operation
   */
  val NOP: String = "EA"
  
  /**
   * Break
   */
  val BRK: String = "00"
  
  /**
   * Compare a byte in memory to the X reg. Sets the Z flag if equal.
   */
  val CPX: String = "EC"
  
  /**
   * Branch X bytes if Z flag = 0
   */
  val BNE: String = "D0"
  
  /**
   * Increment the value of a byte in memory.
   */
  val INC: String = "EE"
  
  /**
   * System call. 
   *  #01 in X reg - Print the integer stored in the Y register.
   *  #02 in X reg - Print the 00-terminated stringstored at the address in the
   *                 y register.
   */
  val SYS: String = "FF"
}
