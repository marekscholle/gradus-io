package com.marekscholle.gradus

import scala.annotation.tailrec
import java.util.NoSuchElementException

object Prerequisities:

  trait Animal:
    def name: String
    def makeSound: String
    def introduceSelf(): Unit =
      println(s"Hi! I'm $name and I do '$makeSound'!")

  object Animal:
    val bob = new Animal:
      def name: String = "Bob"
      def makeSound: String = "Woof"

  sealed trait Option[A]:
    def get: A
    def map[B](f: A => B): Option[B]

  object Option:
    case class None[A]() extends Option[A]:
      def get: A = throw NoSuchElementException()
      def map[B](f: A => B): None[B] = None()

    case class Some[A](value: A) extends Option[A]:
      def get: A = value
      def map[B](f: A => B): Some[B] = Some(f(value))

    def patternMatch: Unit =
      val opt: Option[Any] = ???
      opt match
        case Some(a) =>
        case None()  =>

  object StackOverflowSum:
    //@tailrec
    def sum(n: Int): Long =
      if (n == 0) 0
      else sum(n - 1) + n

  object ImperativeSum:
    def sum(n: Int): Long =
      var acc = 0L
      for { i <- 1 to n } acc += i
      acc

  object TailRecursiveSum:
    def sum(n: Int): Long =
      @tailrec
      def loop(n: Int, acc: Long): Long =
        if (n == 0) acc
        else loop(n - 1, n + acc)
      loop(n, acc = 0L)

  @main def Prerequisities_main(): Unit =
    import Utils._
    "Bob" >>> { Animal.bob.introduceSelf() }
    "stack sum" >>> { StackOverflowSum.sum(10) == 55 }
    "stack sum overflow" >>> {
      try {
        StackOverflowSum.sum(1_000_000)
      } catch {
        case _: StackOverflowError => "overflowed as expected"
      }
    }
    "imperative sum" >>> {
      ImperativeSum.sum(10) == 55 &&
      ImperativeSum.sum(1_000_000) == 500000500000L
    }
    "tail recursive sum" >>> {
      TailRecursiveSum.sum(10) == 55 &&
      TailRecursiveSum.sum(1_000_000) == 500000500000L
    }
