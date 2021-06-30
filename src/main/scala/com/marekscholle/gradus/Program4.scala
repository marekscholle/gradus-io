package com.marekscholle.gradus

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import com.marekscholle.gradus.Program4.FunctionChain.Nonempty
import com.marekscholle.gradus.Program4.FunctionChain

sealed trait Program4[A]:
  def map[B](f: A => B): Program4[B] = Program4.Map(this, f)
  def flatMap[B](f: A => Program4[B]): Program4[B] = Program4.FlatMap(this, f)

object Program4:
  /** Program which (when executed) returns already existing value. */
  case class Ready[A](a: A) extends Program4[A]

  /** Program which (when executed) runs the saved function `f` and returns its result. */
  case class Exec[A](f: () => A) extends Program4[A]

  /** Program which (when executed) runs the saved `program`, takes the result, applies `f`
    * on it (i.e. re-maps the result of `program`), and returns its result.
    */
  case class Map[A, B](
      program: Program4[A],
      f: A => B,
  ) extends Program4[B]

  /** Program which (when executed) runs the saved `program`, applies `f` on it to get
    * another program, runs it and returns its result.
    */
  case class FlatMap[A, B](
      program: Program4[A],
      f: A => Program4[B],
  ) extends Program4[B]

  def suspend[A](program: () => Program4[A]) = Ready(()).flatMap(_ => program())

  def run1[A](program: Program4[A]): A =
    program match {
      case Ready(a) =>
        a
      case Exec(f) =>
        f()
      case Map(program1, f) =>
        val a1 = run1(program1) // not a tail call
        f(a1)

      case FlatMap(program1, f) =>
        val a1 = run1(program1) // not a tail call
        val program2 = f(a1)
        run1(program2)
    }

  sealed trait Todo[B]
  case class More[A, B](program: Program4[A], todo: A => Todo[B]) extends Todo[B]
  case class Done[B](b: B) extends Todo[B]

  @tailrec
  def loop2[A, B](program: Program4[A], todo: A => Todo[B]): B =
    program match {
      case Ready(a) =>
        todo(a) match {
          case Done(b)             => b
          case More(program, todo) => loop2(program, todo)
        }
      case Exec(g) =>
        todo(g()) match {
          case Done(b)             => b
          case More(program, todo) => loop2(program, todo)
        }
      case Map(program, g) =>
        loop2(program, x => todo(g(x)))
      case FlatMap(program, g) =>
        loop2(
          program,
          x => More(g(x), todo),
        )
    }

  def run2[A](program: Program4[A]): A =
    logger.debug(s"run2 #${Thread.currentThread.getStackTrace.size}")
    loop2(program, a => Done(a))

  @tailrec
  def run3[A](program: Program4[A]): A =
    program match {
      case Ready(a) =>
        a

      case Exec(f) =>
        f()

      case Map(program1, f) =>
        val program2 = program1.flatMap(a => Ready(f(a)))
        run3(program2)

      case FlatMap(program1, f) =>
        program1 match {
          case Ready(a) =>
            run3(f(a))
          case Exec(g) =>
            run3(f(g()))
          case Map(program11, g) =>
            run3(FlatMap(program11, g andThen f))
          case FlatMap(program11, g) =>
            run3(FlatMap(program11, a => FlatMap(g(a), f)))
        }
    }

  val logger = LoggerFactory.getLogger(getClass)

  /** Computes the length of (3n+1)-sequence for given `n`.
    *
    * https://en.wikipedia.org/wiki/Collatz_conjecture
    */
  def collatz(n: BigInt): Program4[BigInt] =
    logger.debug(
      s"called collatz($n), stack depth: ${Thread.currentThread.getStackTrace.size}",
    )
    suspend { () =>
      logger.debug(s"collatz($n), stack depth: ${Thread.currentThread.getStackTrace.size}")
      if (n == 1) Ready(0)
      else {
        if (n % 2 == 0) collatz(n / 2).map(_ + 1)
        else collatz(3 * n + 1).map(_ + 1)
      }
    }

  def print(n: BigInt) = Exec { () => println(s"Result: $n") }

  @main def entry41(): Unit =
    run1(collatz(18).flatMap(print))

  @main def entry42(): Unit =
    run2(collatz(18).flatMap(print))

  @main def entry43(): Unit =
    run3(collatz(18).flatMap(print))
