package com.mle.fpis

/**
 * @author Michael
 */
case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String) = copy(stack = (loc, msg) :: stack)

  def label[A](msg: String): ParseError = ParseError(latestLoc.map((_, msg)).toList)

  def latestLoc: Option[Location] = latest map (_._1)

  def latest: Option[(Location, String)] = stack.headOption
}
