package com.evolutiongaming.bootcamp.homework._1

import scala.annotation.tailrec

object _1 {
  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)
  def gcd(a: Int, b: Int): Int = {
    @tailrec
    def gcmRec(a: Int, b: Int): Int = a - b match {
      case 0 => a
      case s if s > 0 => gcmRec(s, b)
      case _ => gcmRec(b - a, a)
    }
    gcmRec(a, b)
  }
}
