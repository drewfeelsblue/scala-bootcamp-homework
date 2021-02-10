package com.evolutiongaming.bootcamp.basics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import LinkedList._
import com.evolutiongaming.bootcamp.basics.Leetcode.{
  kidsWithCandies,
  maxWidthOfVerticalArea,
  maximumWealth,
  runningSum,
  shuffle
}

import scala.util.Random

class CollectionsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "findGap" should "find gap" in {
    findGap(List(1, 2, 3, 5, 6)) shouldEqual Some(3, 5)
  }

  "findGap" should "work correctly on empty" in {
    findGap(List.empty) shouldEqual None
  }

  "findGap" should "work correctly on no gaps" in {
    findGap((1 to 100).toList) shouldEqual None
  }

  "min" should "work correctly on empty" in {
    min(Nil) shouldEqual None
  }

  "min" should "work correctly on non empty" in {
    min(Random.shuffle(1 to 100).toList) shouldEqual Some(1)
  }

  "scanLeft" should "work correctly on numbers" in {
    val numbers = (1 to 100).toList
    scanLeft(0)(numbers)(_ + _) shouldEqual numbers.scanLeft(0)(_ + _)
  }

  "scanLeft" should "work correctly on letters" in {
    val letters = ('a' to 'z').toList.map(_.toString)
    scanLeft("")(letters)(_ + _) shouldEqual letters.scanLeft("")(_ + _)
  }

  "count" should "pass" in {
    count("aaaabbbcca") shouldEqual List(('a', 4), ('b', 3), ('c', 2), ('a', 1))
  }

  "runningSum" should "calculate running sum" in {
    runningSum(List(1, 1, 1, 1, 1)) shouldEqual List(1, 2, 3, 4, 5)
    runningSum(List(1, 2, 3, 4)) shouldEqual List(1, 3, 6, 10)
    runningSum(List(3, 1, 2, 10, 1)) shouldEqual List(3, 4, 6, 16, 17)
  }

  "shuffle" should "work correctly" in {
    shuffle(List(2, 5, 1, 3, 4, 7), 3) shouldEqual List(2, 3, 5, 4, 1, 7)
    shuffle(List(1, 2, 3, 4, 4, 3, 2, 1), 4) shouldEqual List(1, 4, 2, 3, 3, 2, 4, 1)
    shuffle(List(1, 1, 2, 2), 2) shouldEqual List(1, 2, 1, 2)
  }

  "maximumWealth" should "work correctly" in {
    maximumWealth(List(List(1, 2, 3), List(3, 2, 1))) shouldEqual 6
    maximumWealth(List(List(1, 5), List(7, 3), List(3, 5))) shouldEqual 10
  }

  "kidsWithCandies" should "work correctly" in {
    kidsWithCandies(List(2, 3, 5, 1, 3), 3) shouldEqual List(true, true, true, false, true)
    kidsWithCandies(List(4, 2, 1, 1, 2), 1) shouldEqual List(true, false, false, false, false)
    kidsWithCandies(List(12, 1, 12), 10) shouldEqual List(true, false, true)
  }

  "maxWidthOfVerticalArea" should "work correctly" in {
    maxWidthOfVerticalArea(List(List(8, 7), List(9, 9), List(7, 4), List(9, 7))) shouldEqual 1
    maxWidthOfVerticalArea(List(List(3, 1), List(9, 0), List(1, 0), List(1, 4), List(5, 3), List(8, 8))) shouldEqual 3
  }
}
