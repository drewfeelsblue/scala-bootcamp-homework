package com.evolutiongaming.bootcamp.homework._3

import cats.implicits.toTraverseOps
import com.evolutiongaming.bootcamp.homework._3._3.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source
import scala.util.Try
import cats.syntax.either._
import com.evolutiongaming.bootcamp.homework._3._3.ErrorMessage.{IllegalNumberOfArguments, UnsupportedOperation}

object _3 {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  implicit class TryOps[A](ta: Try[A]){
    def toEitherNarrow[B](implicit fromThrowable: Throwable => B): Either[B, A] = ta.toEither.leftMap(fromThrowable)
  }

  sealed trait Command
  object Command {
    final case class Divide private(dividend: Double, divisor: Double) extends Command
    object Divide {
      def apply(dividend: String, divisor: String): Either[ErrorMessage, Divide] = {
        (for {
          dividend <- Try(dividend.toDouble)
          divisor <- Try(divisor.toDouble)
        } yield Divide(dividend, divisor)).toEitherNarrow[ErrorMessage]
      }
    }
    final case class Sum private(numbers: List[Double]) extends Command
    object Sum {
      def apply(numbersS: List[String]): Either[ErrorMessage, Sum] =
        numbersS.traverse(s => Try(s.toDouble).toEitherNarrow[ErrorMessage]).map(Sum.apply)
    }
    final case class Average(numbers: List[Double]) extends Command
    object Average {
      def apply(numbersS: List[String]): Either[ErrorMessage, Average] =
        numbersS.traverse(s => Try(s.toDouble).toEitherNarrow[ErrorMessage]).map(Average.apply)
    }
    final case class Min(numbers: List[Double]) extends Command
    object Min {
      def apply(numbersS: List[String]): Either[ErrorMessage, Min] =
        numbersS.traverse(s => Try(s.toDouble).toEitherNarrow[ErrorMessage]).map(Min.apply)
    }
    final case class Max(numbers: List[Double]) extends Command
    object Max {
      def apply(numbersS: List[String]): Either[ErrorMessage, Max] =
        numbersS.traverse(s => Try(s.toDouble).toEitherNarrow[ErrorMessage]).map(Max.apply)
    }
  }

  sealed abstract class ErrorMessage(details: String) { def message: String = s"Error: $details"}
  object ErrorMessage {
    case object IllegalNumberFormat extends ErrorMessage("Illegal number format")
    case object IllegalNumberOfArguments extends ErrorMessage("Illegal number of arguments")
    case object UnsupportedOperation extends ErrorMessage("Unsupported operation")
    case object UnknownError extends ErrorMessage("Something wen wrong")
    implicit def fromThrowable(throwable: Throwable): ErrorMessage = throwable match {
      case _: NumberFormatException => IllegalNumberFormat
      case _ => UnknownError
    }
  }

  sealed trait Result {
    def result: Double
    def resultPrefix: String
    def print: String = s"$resultPrefix is $result"
  }
  object Result {
    final case class Divide(divide: Command.Divide, result: Double) extends Result {
      override def resultPrefix: String = s"${divide.dividend} divided by ${divide.divisor}"
    }
    final case class Sum(sum: Command.Sum, result: Double) extends Result {
      override def resultPrefix: String = s"the sum of ${sum.numbers.mkString(" ")}"
    }
    final case class Average(average: Command.Average, result: Double) extends Result {
      override def resultPrefix: String = s"the average of ${average.numbers.mkString(" ")}"
    }
    final case class Min(min: Command.Min, result: Double) extends Result {
      override def resultPrefix: String = s"the minimum of ${min.numbers.mkString(" ")}"
    }
    final case class Max(max: Command.Max, result: Double) extends Result {
      override def resultPrefix: String = s"the maximum of ${max.numbers.mkString(" ")}"
    }
  }

  def parseCommand(line: String): Either[ErrorMessage, Command] = {
    line split "\\s+" toList match {
      case "divide" :: dividend :: divisor :: Nil => Divide(dividend, divisor)
      case "divide" :: _ => Left(IllegalNumberOfArguments)
      case "sum" :: args if args.nonEmpty => Sum(args)
      case "sum" :: _ => Left(IllegalNumberOfArguments)
      case "average" :: args if args.nonEmpty => Average(args)
      case "average" :: _ => Left(IllegalNumberOfArguments)
      case "min" :: args if args.nonEmpty => Min(args)
      case "min" :: _ => Left(IllegalNumberOfArguments)
      case "max" :: args if args.nonEmpty => Max(args)
      case "max" :: _ => Left(IllegalNumberOfArguments)
      case _ => Left(UnsupportedOperation)
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = command match {
    case div @ Divide(dividend, divisor) => Try(dividend / divisor)
      .toEitherNarrow[ErrorMessage]
      .map(d => Result.Divide(div, d))
    case s @ Sum(args) => Right(Result.Sum(s, args.sum))
    case a @ Average(args) => Right(Result.Average(a, args.sum/args.size))
    case mi @ Min(args) => Right(Result.Min(mi, args.min))
    case ma @ Max(args) => Right(Result.Max(ma, args.max))
  }

  def renderResult(result: Result): String = result.print

  def process(line: String): String = {
    val renderedRes = for {
      parsedCommand <- parseCommand(line)
      calculated <- calculate(parsedCommand)
      rendered = renderResult(calculated)
    } yield rendered
    renderedRes leftMap { _.message } merge
  }

  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map { line => process(line) } foreach println
}
