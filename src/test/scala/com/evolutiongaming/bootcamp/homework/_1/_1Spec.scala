package com.evolutiongaming.bootcamp.homework._1

import _1._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.mgit commit -m "first commit"atchers.should.Matchers._

class _1Spec extends AnyFlatSpec {

  "gcd" should "return 2" in {
    gcd(2, 4) shouldEqual 2
  }

  "gcd" should "return 11" in {
    gcd(121, 33) shouldEqual 11
  }

  "gcd" should "return 1" in {
    gcd(17, 5) shouldEqual 1
  }
}
