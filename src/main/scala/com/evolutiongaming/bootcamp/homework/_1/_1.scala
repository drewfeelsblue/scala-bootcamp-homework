package com.evolutiongaming.bootcamp.homework._1

import scala.annotation.tailrec

object _1 {
  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)
  def gcd(a: Int, b: Int): Int = {
    @tailrec
    def gcdRec(a: Int, b: Int): Int = a - b match {
      case 0 => a
      case s if s > 0 => gcdRec(s, b)
      case _ => gcdRec(b - a, a)
    }
    gcdRec(a, b)
  }
}
