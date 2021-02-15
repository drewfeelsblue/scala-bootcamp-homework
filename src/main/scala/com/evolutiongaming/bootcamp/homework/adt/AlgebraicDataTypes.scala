package com.evolutiongaming.bootcamp.homework.adt

import cats.implicits.toTraverseOps

object AlgebraicDataTypes {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  import DomainError._

  sealed abstract class DomainError(val message: String)
  object DomainError {
    final case class UnrecognizedSuit(s: Char) extends DomainError(s"Unrecognized suit: $s")
    final case class UnrecognizedRank(r: Char) extends DomainError(s"Unrecognized rank: $r")
    final case class UnrecognizedCard(card: String) extends DomainError(s"Unrecognized card: $card")
    final case class UnrecognizedBoard(board: String) extends DomainError(s"Unrecognized card: $board")
    final case class UnrecognizedTexasHoldemHand(thHand: String) extends DomainError(s"Unrecognized card: $thHand")
    final case class UnrecognizedOmahaHoldemHand(ohHand: String) extends DomainError(s"Unrecognized card: $ohHand")
  }

  sealed trait Suit
  object Suit {
    case object Diamonds extends Suit
    case object Clubs extends Suit
    case object Hearts extends Suit
    case object Spades extends Suit

    def from: Char => Either[DomainError, Suit] = {
      case 'd' => Right(Diamonds)
      case 'c' => Right(Clubs)
      case 'h' => Right(Hearts)
      case 's' => Right(Spades)
      case c   => Left(UnrecognizedSuit(c))
    }
  }

  sealed abstract class Rank(val power: Int)
  object Rank {
    case object A extends Rank(1)
    case object K extends Rank(2)
    case object Q extends Rank(3)
    case object J extends Rank(4)
    case object T extends Rank(5)
    case object _9 extends Rank(6)
    case object _8 extends Rank(7)
    case object _7 extends Rank(8)
    case object _6 extends Rank(9)
    case object _5 extends Rank(10)
    case object _4 extends Rank(11)
    case object _3 extends Rank(12)
    case object _2 extends Rank(13)

    private val allRanks = Map(
      'A' -> A,
      'K' -> K,
      'Q' -> Q,
      'J' -> J,
      'T' -> T,
      '9' -> _9,
      '8' -> _8,
      '7' -> _7,
      '6' -> _6,
      '5' -> _5,
      '4' -> _4,
      '3' -> _3,
      '2' -> _2
    )

    def from(c: Char): Either[DomainError, Rank] = allRanks.get(c).toRight(UnrecognizedRank(c))
  }

  sealed abstract case class Card private (rank: Rank, suit: Suit)
  object Card {
    def from(s: String): Either[DomainError, Card] = s.toList match {
      case r :: s :: Nil =>
        for {
          rank <- Rank.from(r)
          suit <- Suit.from(s)
        } yield new Card(rank, suit) {}
      case _ => Left(UnrecognizedCard(s))
    }
  }

  sealed abstract case class Board private (fc: Card, sc: Card, tc: Card, foc: Card, fic: Card, sRepr: String)
  object Board {
    def from(s: String): Either[DomainError, Board] = s.grouped(2).toList match {
      case first :: second :: third :: fourth :: fifth :: Nil =>
        for {
          fc <- Card.from(first)
          sc <- Card.from(second)
          tc <- Card.from(third)
          foc <- Card.from(fourth)
          fic <- Card.from(fifth)
        } yield new Board(fc, sc, tc, foc, fic, s) {}
      case _ => Left(UnrecognizedBoard(s))
    }
  }

  sealed trait Hand {
    def sRepr: String
  }
  object Hand {
    sealed abstract case class TexasHoldem private (first: Card, second: Card, sRepr: String) extends Hand
    object TexasHoldem {
      def from(str: String): Either[DomainError, TexasHoldem] = str.grouped(2).toList match {
        case first :: second :: Nil =>
          for {
            f <- Card.from(first)
            s <- Card.from(second)
          } yield new TexasHoldem(f, s, str) {}
        case _ => Left(UnrecognizedTexasHoldemHand(str))
      }
    }
    sealed abstract case class OmahaHoldem private (f: Card, s: Card, t: Card, fo: Card, sRepr: String) extends Hand
    object OmahaHoldem {
      def from(str: String): Either[DomainError, OmahaHoldem] = str.grouped(2).toList match {
        case first :: second :: third :: fourth :: Nil =>
          for {
            f <- Card.from(first)
            s <- Card.from(second)
            t <- Card.from(third)
            fo <- Card.from(fourth)
          } yield new OmahaHoldem(f, s, t, fo, str) {}
        case _ => Left(UnrecognizedOmahaHoldemHand(str))
      }
    }
  }

  sealed trait TestCase[H <: Hand] {
    def board: Board
    def hands: List[H]
  }
  object TestCase {
    sealed abstract case class OmahaHoldem(board: Board, hands: List[Hand.OmahaHoldem]) extends TestCase[Hand.OmahaHoldem]
    object OmahaHoldem {
      def from(board: String, hands: List[String]): Either[DomainError, OmahaHoldem] =
        for {
          board <- Board.from(board)
          hands <- hands.traverse(Hand.OmahaHoldem.from)
        } yield new OmahaHoldem(board, hands) {}
    }

    sealed abstract case class TexasHoldem(board: Board, hands: List[Hand.TexasHoldem]) extends TestCase[Hand.TexasHoldem]
    object TexasHoldem {
      def from(board: String, hands: List[String]): Either[DomainError, TexasHoldem] =
        for {
          board <- Board.from(board)
          hands <- hands.traverse(Hand.TexasHoldem.from)
        } yield new TexasHoldem(board, hands) {}
    }
  }

  sealed abstract case class TestResult private (board: String, hands: List[String])
  object TestResult {
    def of[H <: Hand](tc: TestCase[H]): TestResult =
      new TestResult(tc.board.sRepr, tc.hands.sortBy(h => PokerCombination.of(tc.board, h)).map(_.sRepr)) {}
  }

  sealed abstract class PokerCombination(val power: Int)
  object PokerCombination {
    sealed abstract case class HighCard private (kickers: List[Rank]) extends PokerCombination(9)
    sealed abstract case class Pair private (pair: Rank, kickers: List[Rank]) extends PokerCombination(8)
    sealed abstract case class TwoPairs private (fPair: Rank, sPair: Rank, kicker: Rank) extends PokerCombination(7)
    sealed abstract case class ThreeOfAKind private (tripletRank: Rank, kickers: List[Rank]) extends PokerCombination(6)
    sealed abstract case class Straight private (hRank: Rank) extends PokerCombination(5)
    sealed abstract case class Flush private (ranks: List[Rank]) extends PokerCombination(4)
    sealed abstract case class FullHouse private (tripletRank: Rank, pair: Rank) extends PokerCombination(3)
    sealed abstract case class FourOfAKind private (quartetRank: Rank, kicker: Rank) extends PokerCombination(2)
    sealed abstract case class StraightFlush private (hRank: Rank) extends PokerCombination(1)
    object StraightFlush {
      def unapply(tuple: (Board, Hand)): Option[StraightFlush] = ???
    }

    // `unapply` method should be implemented in companion objects for all combinations
    // `of` method should contain cases for all possible combinations
    def of(board: Board, hand: Hand): PokerCombination = (board, hand) match {
      case StraightFlush(sf) => sf
    }

    implicit val ordering: Ordering[PokerCombination] = (x: PokerCombination, y: PokerCombination) => ???
  }

}
