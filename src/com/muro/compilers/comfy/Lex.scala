
package com.muro.compilers.comfy

import com.muro.compilers.comfy.grammar.Tag._
import com.muro.compilers.comfy.grammar._
import com.muro.compilers.comfy.exceptions.InvalidSyntaxException

import scala.collection.mutable.Stack
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

  /**
   * This method is the workhorse that performs most of the tasks required for
   * lexing an input file.
   */
  def tokenize(f: String): Unit = {

    /**
     * Character buffer to store input awaiting lexical analysis.
     */
    val buffer: StringBuilder = new StringBuilder()

    /**
     * A stack for ensuring that all braces are matched.
     */
    val braces: Stack[String] = new scala.collection.mutable.Stack[String]

    /**
     * A stack for ensuring that all parenthesis are matched.
     */
    val parens: Stack[String] = new scala.collection.mutable.Stack[String]

    /**
     * Contains the source code read from the input file.
     */
    var source: StringBuilder = new StringBuilder()

    /**
     * An iterator for traversing the source code character-by-character.
     * A BufferedIterator provides the head method which allows one-character
     * look-ahead.
     */
    var tokenStream: BufferedIterator[Char] = null

    /**
     * Tracks the line number for error reporting purposes.
     */
    var nLines = 1

    /**
     * Tracks the column number for error reporting.
     */
    var nColumns = 1

    // Read the input file contained at f line-by-line.
    Source.fromFile(new File(f)).getLines.foreach { line =>
      // Strip leading and trailing whitespace and reduce extraneous whitespace.
      source.append(line.trim().replaceAll("(\\s){2,}", " ") + "\n")
    }

    // Strip leading and trailing newlines.
    source = new StringBuilder(source.toString().trim())
    
    // Does the file contain any text?
    if (source.isEmpty) {
      throw new InvalidSyntaxException("No source code.")
    }
    
    
    // Verify that the program begins with an opening brace.
    if (source.charAt(0) != '{') {
      throw new InvalidSyntaxException("Program should begin with an opening brace.")
    }

    // Verify that the program contains the end-of-program marker.
    if (source.charAt(source.length - 1) != '$') {
      source.append("$");
      doWarn("Missing end-of-program marker. Inserting end-of-program marker" +
        " as a courtesy.\nPlease make sure that you end your program " +
        "properly next time.");
    }

    // An iterator will allow the source to be parsed character-by-character. 
    tokenStream = source.iterator.buffered

    // Loop until there are no more characters.
    while (tokenStream.hasNext) {
      consume(tokenStream.next)
      nColumns += 1
    }

    if (!parens.isEmpty || !braces.isEmpty)
      throw new InvalidSyntaxException("Unmatched brace or parenthesis found.")

    /**
     * Consumes a single character of the source input program. If c is an
     * alphanumeric character it is buffered, otherwise the character is
     * checked to determine if it is a valid lexeme. Alphanumeric characters
     * are the only terminals that contain multiple characters in a row,
     * except for Strings, which are handled differently.
     */
    def consume(c: Char): Unit = {
      // Buffer all alpha-numeric characters.
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

    /**
     * Determines if the specified String matches a lexeme pattern defined by
     * our grammar. If so, the lexeme is extracted and added to our token queue.
     */
    def identifyLexeme(terminal: String): Unit = {

      terminal match {
        case Pattern.OpenBrace() => {
          tokens.enqueue(new Token(T_openBrace, nLines))
          braces.push("{")
        }
        case Pattern.CloseBrace() => {
          tokens.enqueue(new Token(T_closeBrace, nLines))
          braces.pop()
        }
        case Pattern.OpenParen() => {
          tokens.enqueue(new Token(T_openParen, nLines))
          parens.push("(")
        }
        case Pattern.CloseParen() => {
          tokens.enqueue(new Token(T_closeParen, nLines))
          parens.pop()
        }
        case Pattern.Plus() => {
          tokens.enqueue(new Token(T_plusOp, nLines, "+"))
        }
        case Pattern.AssignmentOp() => {
          // Look-ahead to determine if this is the assignment operator or 
          // the boolean equality operator.
          if (tokenStream.head == '=') {
            tokens.enqueue(new Token(T_boolOp, nLines, "=="))
            tokenStream.next
            nColumns += 1
          } else {
            tokens.enqueue(new Token(T_assignOp, nLines))
          }
        }
        case Pattern.NotOperator() => {
          if (tokenStream.head == '=') {
            tokens.enqueue(new Token(T_boolOp, nLines, "!="))
            tokenStream.next
            nColumns += 1;
          } else {
            throw new InvalidSyntaxException(
              "Expecting equals sign to complete boolean not equals " +
              "operator on line " + nLines)
          }
        }
        case Pattern.DoubleQuote() => {
          
          tokens.enqueue(new Token(T_dblQuote, nLines))
          
          val charList: StringBuilder = new StringBuilder("")
            
          // Process all characters contained within the double-quotes.
          while (tokenStream.head != '"') {
            // Alphanumeric and space characters are valid. 
            if (tokenStream.head.isLetterOrDigit || 
                tokenStream.head.isSpaceChar) {
                  charList.append(tokenStream.next)
            } else if (tokenStream.head.isControl) {
              // We encountered a line termination character, which is bad...
              // because our grammar does not allow multi-line Strings.
              throw new InvalidSyntaxException("Line termination character " +
                "found while processing String on line " + nLines + 
                ". Multi-line Strings are not supported.")
            } else {
              throw new InvalidSyntaxException(
                "Invalid String literal found on line " + nLines + ". Strings" +
                " can only contain alphanumeric characters and spaces.")
            }
          }
          
          // Dispose of the closing double-quote.
          tokenStream.next
          tokens.enqueue(new Token(T_stringLiteral, nLines, charList.toString))
          
          tokens.enqueue(new Token(T_dblQuote, nLines))
        }
        case Pattern.Space() => {
          // do nothing right now
        }
        case Pattern.Newline() => {
          nLines += 1
          nColumns = 1
        }
        case Pattern.EndOfProgram() => {
          tokens.enqueue(new Token(T_endOfProgram, nLines))
        }
        case Pattern.If() => {
          tokens.enqueue(new Token(T_if, nLines))
        }
        case Pattern.While() => {
          tokens.enqueue(new Token(T_while, nLines))
        }
        case Pattern.Print() => {
          tokens.enqueue(new Token(T_print, nLines))
        }
        case Pattern.BoolLiteral() => {
          tokens.enqueue(new Token(T_boolLiteral, nLines, buffer.toString))
        }
        case Pattern.IntLiteral() => {
          tokens.enqueue(new Token(T_numLiteral, nLines, buffer.toString))
        }
        case Pattern.Int() => {
          tokens.enqueue(new Token(T_type, nLines, "int"))
        }
        case Pattern.Boolean() => {
          tokens.enqueue(new Token(T_type, nLines, "boolean"))
        }
        case Pattern.String() => {
          tokens.enqueue(new Token(T_type, nLines, "string"))
        }
        case Pattern.Id() => {
          tokens.enqueue(new Token(T_id, nLines, buffer.toString))
        }
        case _ => {
          throw new InvalidSyntaxException(
            "Unrecognized character sequence \"" + terminal + "\" found on line " + nLines)
        }
      }
    }
    
    /**
     * Prints a warning message to the console.
     */
    def doWarn(msg: String) {
      println("WARNING: " + msg + "\nLINE: " + nLines + " POSITION: " + nColumns)
    }
  }
  
  def main(args: Array[String]): Unit = {
    tokenize(args(0))
    tokens.foreach { token => println(token.tag + " " + token.attr) }
  }
}