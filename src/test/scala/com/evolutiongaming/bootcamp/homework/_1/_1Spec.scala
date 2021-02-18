package com.evolutiongaming.bootcamp.homework._1

import _1._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

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

  "gcd" should "return non-zero argument if another one is zero" in {
    gcd(0, 5) shouldEqual 5
    gcd(5, 0) shouldEqual 5
  }

  "gcd" should "raise an exception" in {
    the[IllegalArgumentException] thrownBy gcd(0, 0) should have message "At least one argument should be non-zero"
  }
}
