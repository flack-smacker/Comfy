
package com.muro.compilers.comfy

import scala.io._
import java.io._

/**
 * Lex is responsible for reading in a file containing a source program,
 * scanning the source and identifying lexemes that match specific token
 * patterns, constructing tokens from said lexems, and outputting a stream of
 * tokens to be processed during the parse phase.
 */
object Lex {

  /**
   * Contains the tokens extracted from the source input.
   */
  val tokens = new scala.collection.mutable.Queue[Token]

  var nLetters: Int = 0
  var nSpaces: Int = 0
  var nNewlines: Int = 0
  var nPunctuation: Int = 0
  var nDigits = 0
  var nUnclassified = 0

  def tokenize(f: String): Unit = {

    val source: StringBuilder = new StringBuilder()

    // Read the input file contained at f line-by-line.
    Source.fromFile(new File(f)).getLines.foreach { line =>
      // Strip leading and trailing whitespace and reduce extraneous whitespace.
      source.append(line.trim().replaceAll("(\\s)(2,}", " ") + "\n")
    }

    // Verify that the program begins with an opening brace.
    if (source.charAt(0) != '{') {
      source.insert(0, "{")
      doWarn("Missing opening brace. Inserting opening brace.")
    }

    // Verify that the program contaisn the end-of-program marker.
    if (source.charAt(source.length - 1) != '$') {
      source.append("$");
      doWarn("Missing end-of-program marker. Inserting opening marker");
    }

    Source.fromIterable(source).foreach { c =>
      consume(c)
    }

    def consume(c: Char): Unit = {

      /**
       * Contains the characters awaiting lexical analysis.
       */
      val buffer = new scala.collection.mutable.ArrayBuffer[Char]
      var nLines = 1
      var nColumns = 1

      if (c.isDigit) {
        buffer.append(c)
        nDigits += 1
      } else if (c.isLetter) {
        buffer.append(c)
        nLetters += 1
      } else if (c.isSpaceChar) {
        // TODO: Check the buffer.
        nSpaces += 1
      } else if (c == "{") {

      } else if (c == "(") {
        // Check that the prior token was a print, while, or if token
      } else if (c == '\n') {
        // TODO: Check the buffer.
        nLines += 1
      } else {
        System.out.print(" " + c)
      }
    }

    def doWarn(msg: String) {
      println("WARNING: " + msg);
    }

    System.out.println("\nLetters: " + nLetters)
    System.out.println("Digits: " + nDigits)
    System.out.println("Newlines: " + nNewlines)
    System.out.println("Spaces: " + nSpaces)
    System.out.println("Unclassified: " + nUnclassified)
  }

  def nextToken(): Token = {
    if (hasNextToken())
      tokens.dequeue
    else
      null
  }

  def hasNextToken(): Boolean = {
    !tokens.isEmpty
  }

  def main(args: Array[String]): Unit = {
    tokenize(args(0))
  }
}