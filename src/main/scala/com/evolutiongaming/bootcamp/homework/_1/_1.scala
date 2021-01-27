package com.evolutiongaming.bootcamp.homework._1

import scala.annotation.tailrec

object _1 {
  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)
  @tailrec
  def gcd(a: Int, b: Int): Int = a - b match {
    case _ if a == 0 || b == 0 => throw new IllegalArgumentException("Zero value as an argument")
    case 0 => a
    case s if s > 0 => gcd(s, b)
    case _ => gcd(b - a, a)
  }
}
