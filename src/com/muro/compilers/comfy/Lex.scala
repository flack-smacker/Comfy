
package com.muro.compilers.comfy

import scala.io._
import java.io._

/**
 * Lex is responsible for reading in a file containing a source program,
 * scanning the source and identifying lexemes that match specific token
 * patterns, constructing tokens from said lexems, and outputting a stream of
 * tokens to be parsed.
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
     * A BufferedIterator provides the head method which allows one-character
     * look-ahead.
     */
    var tokenStream: BufferedIterator[Char] = null

    /**
     * Character buffer to store input awaiting lexical analysis.
     */
    val buffer: StringBuilder = new StringBuilder()

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

    // An iterator will allow the source to be parsed character-by-character. 
    tokenStream = source.iterator.buffered

    // Loop until there are no more characters.
    while (tokenStream.hasNext && !fail) {
      consume(tokenStream.next)
      nColumns += 1
    }
    
    /**
     * Consumes a single character of the source input program. If c is an 
     * alphanumeric character it is buffered, otherwise the character is
     * identified as a valid lexeme. Alphanumeric characters are the only
     * terminals that can occur more than once in a row, except for Strings, 
     * which are handled differently.
     */
    def consume(c: Char): Unit = {
      if (c.isLetterOrDigit) {
        buffer.append(c)
      } else {
        // We've hit a non-alphanumeric character.
        if (!buffer.isEmpty) {
          // Check the buffer for a valid lexeme.
          identifyLexeme(buffer.toString)
          buffer.clear()
        }
        // Determine which non-terminal we've hit.
        identifyLexeme(c.toString)
      }
    }
    
    def identifyLexeme(terminal: String): Unit = {

      terminal match {
        case Terminal.OpenBrace() => {
            tokens.enqueue(new Token(Tag.T_openBrace, ""))
            println("OPEN BRACE MATCH")
          }
        case Terminal.CloseBrace() => {
            tokens.enqueue(new Token(Tag.T_closeBrace, ""))
            println("CLOSE BRACE MATCH")
          }
        case Terminal.OpenParen() => {
            tokens.enqueue(new Token(Tag.T_openParen, ""))
            println("OPEN PAREN MATCH")
          }
        case Terminal.CloseParen() => {
            tokens.enqueue(new Token(Tag.T_closeParen, ""))
            println("CLOSE PAREN MATCH")
          }
        case Terminal.Plus() => {
            tokens.enqueue(new Token(Tag.T_plusOp, ""))
            println("PLUS SIGN MATCH")
          }
        case Terminal.Equals() => {
            // Look-ahead to determine if this is the assignment operator or 
            // the boolean equality operator.
            if (tokenStream.head == '=') {
              tokens.enqueue(new Token(Tag.T_boolOp, "=="))
              tokenStream.next
              nColumns += 1
              println("EQUALITY SIGN MATCH")
            } else {
              tokens.enqueue(new Token(Tag.T_assignOp, ""))
              println("ASSIGNMENT MATCH")
            }
          }
        case Terminal.Exclamation() => {
            if (tokenStream.head == '=') {
              tokens.enqueue(new Token(Tag.T_boolOp, "!="))
              tokenStream.next
              nColumns += 1
              println("NOT EQUALS MATCH")
            } else {
              doError("Syntax error. Expecting equals sign to complete not operator.")
            }
        }
        case Terminal.DoubleQuote() => {
            // process string
            println("DOUBLE QUOTE MATCH")
        }
        case Terminal.Space() => {
            println("SPACE FOUND MATCH")
        }
        case Terminal.Newline() => {
            nLines += 1
            nColumns = 1
            println("NEWLINE MATCH")
        }
        case Terminal.EndOfProgram() => {
            println("END OF PROGRAM MATCH")
        }
        case Terminal.If() =>
          tokens.enqueue(new Token(Tag.T_if, ""))
        case Terminal.While() =>
          tokens.enqueue(new Token(Tag.T_while, ""))
        case Terminal.Print() =>
          tokens.enqueue(new Token(Tag.T_print, ""))
        case Terminal.BoolLiteral() =>
          tokens.enqueue(new Token(Tag.T_boolLiteral, buffer.toString))
        case Terminal.Digit() =>
          tokens.enqueue(new Token(Tag.T_numLiteral, buffer.toString))
        case Terminal.Int() =>
          tokens.enqueue(new Token(Tag.T_int, ""))
        case Terminal.Boolean() =>
          tokens.enqueue(new Token(Tag.T_boolean, ""))
        case Terminal.String() =>
          tokens.enqueue(new Token(Tag.T_string, ""))
        case Terminal.Id() =>
          tokens.enqueue(new Token(Tag.T_id, buffer.toString))
        case _ => {
            doError("Syntax error. Invalid character sequence." + terminal)
        }
      }
    }

    def doWarn(msg: String) {
      println("WARNING: " + msg + "\nLINE: " + nLines + "\nCOLUMN: " + nColumns)
    }

    def doError(msg: String) {
      println("ERROR: " + msg + "\nLINE: " + nLines + "\nCOLUMN: " + nColumns)
      fail = true
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