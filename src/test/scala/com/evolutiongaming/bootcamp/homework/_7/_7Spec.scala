package com.evolutiongaming.bootcamp.homework._7

import _7.TypeclassTask.HashCodeSyntax._
import _7.TypeclassTask._
import _7.Task1.Money
import _7.Task1.moneyOrdering
import _7.Task2.{ User => UserTask2 }
import _7.Task2.ShowSyntax._
import _7.Task3.{ User => UserTask3 }
import _7.Task3.ParseSyntax._
import _7.Task3.User.parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class _7Spec extends AnyFlatSpec {

  "hash for String" should "return the same value as for `hashcode` call" in {
    val testStr = "test_string"
    testStr.hash shouldEqual testStr.hashCode
  }

  "hash for Int" should "return value itself" in {
    2.hash shouldEqual 2
  }

  "list of Money" should "be ordered by its inner BigDecimal value" in {
    List(Money(2.1), Money(1.0), Money(0.5)).sorted shouldEqual List(Money(0.5), Money(1.0), Money(2.1))
  }

  "show for User" should "return the same value as simple toString method returns" in {
    val testUser = UserTask2("1", "Oleg")
    testUser.show shouldEqual testUser.toString
  }

  "parse for User" should "return `Format error` for empty string" in {
    "".parse shouldEqual Left("Format error")
  }

  "parse for User" should "return `id is empty`" in {
    " , Oleg".parse shouldEqual Left("id is empty")
  }

  "parse for User" should "return `name is empty`" in {
    " 1, ".parse shouldEqual Left("name is empty")
  }

  "parse for User" should "return User instance" in {
    " 1, Oleg ".parse shouldEqual UserTask3.of("1", "Oleg")
  }
}
