package com.mle.fpis

import com.mle.fpis.MyParser.Parser
import com.mle.fpis.Parsers.{Failure, Result, Success}

import scala.util.matching.Regex

/**
 * @author Michael
 */
object MyParser {
  type Parser[+A] = Location => Result[A]
}

object MyParsers extends Parsers[MyParser.Parser] {

  override def run[A](p: Parser[A])(input: Location): Result[A] = p(input)

  def failingParser(error: ParseError): Parser[Nothing] = loc => Failure(error, isCommitted = false)

  override implicit def string(s: String): Parser[String] = loc =>
    if (loc.fromOffset startsWith s) Success(s, s.size)
    else Failure(loc.toError(s"Expected: $s"), isCommitted = false)

  override def slice[A](p: Parser[A]): Parser[String] = loc => {
    val remainingInput = loc.fromOffset
    p(loc) match {
      case Success(a, chars) => Success(remainingInput.substring(0, chars), chars)
      case f: Failure => f
    }
  }

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = loc => {
    p(loc) match {
      case Success(a, chars) => f(a)(loc advanceBy chars).addCommit(chars == 0)
      case f: Failure => f
    }
  }

  override implicit def regex(r: Regex): Parser[String] = loc =>
    (r findFirstIn loc.fromOffset)
      .fold[Result[String]](Failure(loc.toError(s"Does not match: $r"), isCommitted = false))(s => Success(s, s.size))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.push(loc, msg))

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.label(msg))

  override def attempt[A](p: Parser[A]): Parser[A] = loc => p(loc).uncommit

  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = loc => p1(loc) match {
    case Failure(error, false) => p2(loc)
    case other => other
  }

  override def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = ???

  //  override def errorMessage(e: ParseError): String = ???
  //
  //  override def errorLocation(e: ParseError): Location = ???

}
