package com.evolutiongaming.bootcamp.homework._7

object _7 {
  object TypeclassTask {
    trait HashCode[A] {
      def hash(a: A): Int
    }

    object HashCode {
      def apply[A: HashCode]: HashCode[A] = implicitly

      def hashCode[A](f: A => Int): HashCode[A] = (a: A) => f(a)

      def fromHashCode[A]: HashCode[A] = (a: A) => a.hashCode()
    }

    object HashCodeSyntax {
      implicit class HashCodeOps[A: HashCode](a: A) {
        def hash: Int = HashCode[A].hash(a)
      }
    }

    implicit val stringHashCode: HashCode[String] = HashCode.fromHashCode

    implicit val intHashCode: HashCode[Int] = HashCode.hashCode(identity)
  }

  object Task1 {
    final case class Money(amount: BigDecimal)
    implicit val moneyOrdering: Ordering[Money] = Ordering[BigDecimal].on(_.amount)
  }

  object Task2 {
    trait Show[T] {
      def show(entity: T): String
    }

    object Show {
      def apply[A: Show]: Show[A] = implicitly
    }

    object ShowSyntax {
      implicit class ShowOps[A: Show](a: A) {
        def show: String = Show[A].show(a)
      }
    }

    final case class User(id: String, name: String)
    object User {
      implicit val userShow: Show[User] = _.toString
    }
  }

  object Task3 {
    type Error = String

    trait Parse[T] {
      def parse(entity: String): Either[Error, T]
    }

    object Parse {
      def apply[A: Parse]: Parse[A] = implicitly
    }

    object ParseSyntax {
      implicit class ParseOps(s: String) {
        def parse[A: Parse]: Either[Error, A] = Parse[A].parse(s)
      }
    }

    sealed abstract case class User private (id: String, name: String)

    object User {
      def of(id: String, name: String): Either[Error, User] = {
        val idT   = id.trim
        val nameT = name.trim
        for {
          id <- Either.cond(idT.nonEmpty, idT, "id is empty")
          name <- Either.cond(nameT.nonEmpty, nameT, "name is empty")
        } yield new User(id, name) {}
      }

      implicit val parser: Parse[User] = (entity: String) =>
        entity.split(",").toList match {
          case id :: name :: Nil => User.of(id, name)
          case _                 => Left("Format error")
        }

    }
  }

  object Task4 {
    trait Eq[A] {
      def isEqual(a1: A, a2: A): Boolean
    }
    object Eq {
      def apply[A: Eq]: Eq[A] = implicitly
    }

    object EqSyntax {
      implicit class EqOps[A: Eq](a: A) {
        def ===(aa: A): Boolean = Eq[A].isEqual(a, aa)
      }
    }

    implicit val stringEq: Eq[String] = _ == _

    import EqSyntax._
    "qwe" === "qwe"
    //1 === 2 will not compile
  }

  object AdvancedHomework {
    trait FlatMap[F[_]] {
      def flatMap[A, B](f: A => F[B])(fa: F[A]): F[B]
    }

    object FlatMap {
      def apply[F[_]: FlatMap]: FlatMap[F] = implicitly
    }

    object FlatMapSyntax {
      implicit class FlatMapOps[F[_]: FlatMap, A](fa: F[A]) {
        def flatMap[B](f: A => F[B]): F[B] = FlatMap[F].flatMap(f)(fa)
      }
    }
  }
}
