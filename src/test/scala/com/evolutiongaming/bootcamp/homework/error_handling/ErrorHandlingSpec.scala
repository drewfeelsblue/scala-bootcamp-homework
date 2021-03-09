package com.evolutiongaming.bootcamp.homework.error_handling

import cats.data.{ Chain, NonEmptyChain }
import com.evolutiongaming.bootcamp.homework.error_handling.ErrorHandling.PaymentCard
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import cats.syntax.validated._
import com.evolutiongaming.bootcamp.homework.error_handling.ErrorHandling.ValidationError._

class ErrorHandlingSpec extends AnyFlatSpec {
  private val validName   = "OLEG IVANOV"
  private val invalidName = "INVALID"

  private val validNumber   = "1111 2222 3333 4444"
  private val invalidNumber = "1111 2222 3333 4444 5555"

  private val validExpirationDate         = "22/05"
  private val invalidFormatExpirationDate = "22-05"
  private val invalidExpirationDate       = "19/05"

  private val validSecurityCode   = "111"
  private val invalidSecurityCode = "1111"

  "payment card builder" should "return name format error" in {
    PaymentCard(invalidName, validNumber, validExpirationDate, validSecurityCode).leftSideValue shouldEqual NameFormatError.invalidNec
  }

  "payment card builder" should "return number format error" in {
    PaymentCard(validName, invalidNumber, validExpirationDate, validSecurityCode).leftSideValue shouldEqual NumberFormatError.invalidNec
  }

  "payment card builder" should "return expiration date format error" in {
    PaymentCard(validName, validNumber, invalidFormatExpirationDate, validSecurityCode).leftSideValue shouldEqual ExpirationDateFormatError.invalidNec
  }

  "payment card builder" should "return card expired error" in {
    PaymentCard(validName, validNumber, invalidExpirationDate, validSecurityCode).leftSideValue shouldEqual CardExpiredError.invalidNec
  }

  "payment card builder" should "return security code format error" in {
    PaymentCard(validName, validNumber, validExpirationDate, invalidSecurityCode).leftSideValue shouldEqual SecurityCodeFormatError.invalidNec
  }

  "payment card builder" should "return constructed object" in {
    PaymentCard(validName, validNumber, validExpirationDate, validSecurityCode).isValid shouldEqual true
  }

  "payment card builder" should "return chain of errors" in {
    PaymentCard(invalidName, validNumber, invalidExpirationDate, validSecurityCode).leftSideValue shouldEqual NonEmptyChain(
      NameFormatError,
      CardExpiredError
    ).invalid
  }
}
