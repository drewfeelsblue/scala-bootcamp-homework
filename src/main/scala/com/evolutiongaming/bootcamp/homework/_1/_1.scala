package com.evolutiongaming.bootcamp.homework._1

import scala.annotation.tailrec

object _1 {
  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)
  @tailrec
  def gcd(a: Int, b: Int): Int = (a, b, a - b) match {
    case (0, 0, 0) => throw new IllegalArgumentException("At least one argument should be non-zero")
    case (_, 0, _) => a
    case (0, _, _) => b
    case (_, _, 0) => a
    case (_, _, s) if s > 0 => gcd(s, b)
    case _ => gcd(b - a, a)
  }
}
