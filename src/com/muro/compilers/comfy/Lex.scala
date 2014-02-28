
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

  def tokenize(f: String): Unit = {

    /**
     * Contains the source code read from the input file.
     */
    val source: StringBuilder = new StringBuilder()

    /**
     * An iterator for traversing the source code character-by-character.
     * A BufferedIterator provides the head method which allows look-ahead.
     */
    var s: BufferedIterator[Char] = null

    /**
     * Character buffer to store input awaiting lexical analysis.
     */
    val buffer: StringBuilder = new StringBuilder();

    /**
     * A flag to indicate an unrecoverable syntax error.
     */
    var fail: Boolean = false

    /**
     * Tracks the line number for error reporting purposes.
     */
    var nLines = 1

    /**
     * Tracks the column number for error reporting purposes.
     */
    var nColumns = 1

    // Read the input file contained at f line-by-line.
    Source.fromFile(new File(f)).getLines.foreach { line =>
      // Strip leading and trailing whitespace and reduce extraneous whitespace.
      source.append(line.trim().replaceAll("(\\s){2,}", " ") + "\n")
    }

    // Verify that the program begins with an opening brace.
    if (source.charAt(0) != '{') {
      source.insert(0, "{")
      doWarn("Missing opening brace. Inserting opening brace.")
    }

    // Verify that the program contains the end-of-program marker.
    if (source.charAt(source.length - 2) != '$') {
      source.append("$");
      doWarn("Missing end-of-program marker. Inserting end-of-program marker.");
    }

    s = source.iterator.buffered

    def consume(c: Char): Unit = {

      c.toString match {
        case Terminal.Digit.r() => {
          buffer.append(c)
        }
        case Terminal.Char.r() => {
          buffer.append(c)
        }
        case Terminal.OpenBrace.r() => {
          tokens.enqueue(new Token(Tag.T_openBrace, ""))
          println("OPEN BRACE MATCH")
        }
        case Terminal.CloseBrace.r() => {
          tokens.enqueue(new Token(Tag.T_closeBrace, ""))
        }
        case Terminal.OpenParen.r() => {
          tokens.enqueue(new Token(Tag.T_openParen, ""))
          checkBuffer()
        }
        case Terminal.CloseParen.r() => {
          tokens.enqueue(new Token(Tag.T_closeParen, ""))
        }
        case Terminal.Plus.r() => {
          tokens.enqueue(new Token(Tag.T_plusOp, ""))
          checkBuffer()
        }
        case Terminal.Equals.r() => {
          // Look-ahead to determine if this is the assignment operator or 
          // the boolean equality operator.
          if (s.head == '=') {
            tokens.enqueue(new Token(Tag.T_boolOp, "=="))
            s.next
          } else {
            tokens.enqueue(new Token(Tag.T_assignOp, ""))
          }
          checkBuffer()
        }
        case Terminal.Exclamation.r() => {
          if (s.head == '=') {
            tokens.enqueue(new Token(Tag.T_boolOp, "!="))
            s.next
          } else {
            doError("Syntax error. Expecting equals sign to complete not operator.")
          }
          checkBuffer()
        }
        case Terminal.DoubleQuote.r() => {
          // process string
        }
        case Terminal.Space.r() => {
          checkBuffer()
        }
        case Terminal.Newline.r() => {
          checkBuffer()
          nLines += 1
          nColumns = 0
        }
        case Terminal.EndOfProgram.r() => {
          println("EndOfProgram: " + c)
        }
        case _ => {
          doError("Syntax error. Input not recognized.")
        }
      }
    }

    def checkBuffer() {
      if (!buffer.isEmpty) {
        buffer.toString match {
          case Terminal.If.r() =>
            tokens.enqueue(new Token(Tag.T_if, ""))
          case Terminal.While.r() =>
            tokens.enqueue(new Token(Tag.T_while, ""))
          case Terminal.Print.r() =>
            tokens.enqueue(new Token(Tag.T_print, ""))
          case Terminal.BoolLiteral.r() =>
            tokens.enqueue(new Token(Tag.T_boolLiteral, buffer.toString))
          case Terminal.Digit.r() =>
            tokens.enqueue(new Token(Tag.T_numLiteral, buffer.toString))
          case Terminal.Int.r() =>
            tokens.enqueue(new Token(Tag.T_int, ""))
          case Terminal.Boolean.r() =>
            tokens.enqueue(new Token(Tag.T_boolean, ""))
          case Terminal.String.r() =>
            tokens.enqueue(new Token(Tag.T_string, ""))
          case Terminal.Id.r() =>
            tokens.enqueue(new Token(Tag.T_id, buffer.toString))
          case _ =>
            doError("Invalid character sequence.")
        }
      }
      // Empty the buffer.
      buffer.clear
    }

    def doWarn(msg: String) {
      println("WARNING: " + msg + "\nLINE: " + nLines + "\nCOLUMN: " + nColumns)
    }

    def doError(msg: String) {
      println("ERROR: " + msg + "\nLINE: " + nLines + "\nCOLUMN: " + nColumns)
      fail = true
    }

    // Loop until there are no more characters.
    while (s.hasNext && !fail) {
      consume(s.next)
      nColumns += 1
    }
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

    while (!tokens.isEmpty) {
      println(tokens.dequeue.tag)
    }
  }
}