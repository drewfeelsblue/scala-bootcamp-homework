package com.evolutiongaming.bootcamp.homework.error_handling

import java.time.LocalDate
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

import cats.data.ValidatedNec
import cats.syntax.validated._
import cats.syntax.apply._

import scala.util.Try

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object ErrorHandling {

  case class HolderName(firstName: String, lastName: String)
  case class CardNumber(first: FourDigitNumber, second: FourDigitNumber, third: FourDigitNumber, fourth: FourDigitNumber)
  case class ExpirationDate(date: LocalDate)
  sealed abstract case class SecurityCode private (securityCode: Int)
  object SecurityCode {
    def apply(i: Int): Option[SecurityCode] =
      if (i >= 100 && i <= 999) Some(new SecurityCode(i) {})
      else None
  }
  sealed abstract case class FourDigitNumber private (i: Int)

  object FourDigitNumber {
    def apply(s: String): Option[FourDigitNumber] =
      s.toIntOption.filter(i => i >= 1000 && i <= 9999).map(new FourDigitNumber(_) {})
    def unapply(str: String): Option[FourDigitNumber] = apply(str)
  }

  sealed abstract case class PaymentCard private (
      holderName: HolderName,
      number: CardNumber,
      expirationDate: ExpirationDate,
      securityCode: SecurityCode
  )

  object PaymentCard {
    import PaymentCardValidator._

    def apply(name: String, number: String, expirationDate: String, securityCode: String): AllErrorsOr[PaymentCard] =
      (
        validateName(name),
        validateNumber(number),
        validateFormatExpirationDate(expirationDate).andThen(validateExpirationPeriod),
        validateSecurityCode(securityCode)
      ).mapN((na, nu, ed, sc) => new PaymentCard(na, nu, ed, sc) {})
  }

  sealed trait ValidationError
  object ValidationError {
    case object NameFormatError extends ValidationError
    case object NumberFormatError extends ValidationError
    case object ExpirationDateFormatError extends ValidationError
    case object CardExpiredError extends ValidationError
    case object SecurityCodeFormatError extends ValidationError
  }

  object PaymentCardValidator {

    import ValidationError._
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    object ContainsOnlyLetters {
      def unapply(str: String): Option[String] = Option.when(str.forall(_.isLetter))(str)
    }

    def validateName(name: String): AllErrorsOr[HolderName] = name.split(" ").toList match {
      case ContainsOnlyLetters(firstName) :: ContainsOnlyLetters(lastName) :: Nil => HolderName(firstName, lastName).validNec
      case _                                                                      => NameFormatError.invalidNec
    }

    def validateNumber(number: String): AllErrorsOr[CardNumber] = number.split(" ").toList match {
      case FourDigitNumber(fi) :: FourDigitNumber(se) :: FourDigitNumber(th) :: FourDigitNumber(fo) :: Nil =>
        CardNumber(fi, se, th, fo).validNec
      case _ => NumberFormatError.invalidNec
    }

    def validateFormatExpirationDate(expirationDate: String): AllErrorsOr[ExpirationDate] =
      Try(
        LocalDate.parse(
          expirationDate,
          new DateTimeFormatterBuilder().parseDefaulting(ChronoField.DAY_OF_MONTH, 1L).appendPattern("yy/MM").toFormatter
        )
      ).fold(_ => ExpirationDateFormatError.invalidNec, ExpirationDate(_).validNec)

    def validateExpirationPeriod(expirationDate: ExpirationDate): AllErrorsOr[ExpirationDate] =
      if (expirationDate.date.isBefore(LocalDate.now())) CardExpiredError.invalidNec else expirationDate.validNec

    def validateSecurityCode(securityCode: String): AllErrorsOr[SecurityCode] =
      securityCode.toIntOption
        .flatMap(SecurityCode.apply)
        .fold(SecurityCodeFormatError.invalidNec[SecurityCode])(_.validNec)
  }
}
