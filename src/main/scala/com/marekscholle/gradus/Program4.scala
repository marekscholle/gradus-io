package com.marekscholle.gradus

import org.slf4j.LoggerFactory
import scala.annotation.tailrec

sealed trait Program4[A]:
  def map[B](f: A => B): Program4[B] = Program4.Map(this, f)
  def flatMap[B](f: A => Program4[B]): Program4[B] = Program4.FlatMap(this, f)

object Program4:
  /** Program which (when interpreted) returns an already existing value. */
  private case class Ready[A](a: A) extends Program4[A]

  /** Program which (when interpreted) runs the `program`, applies `f` on its result (i.e.
    * re-maps the result of `program`), and returns its result.
    */
  private case class Map[A, B](
      program: Program4[A],
      f: A => B,
  ) extends Program4[B]

  /** Program which (when interpreted) runs the `program`, applies `f` on its result to get
    * an _another_ program, runs it and returns its result.
    */
  private case class FlatMap[A, B](
      program: Program4[A],
      f: A => Program4[B],
  ) extends Program4[B]

  /** Program which (when interpreted) returns an already existing value. */
  def ready[A](a: A): Program4[A] = Ready(a)

  /** Program which (when interpreted) runs the function `f` and provides its result. */
  def suspend[A](f: () => A): Program4[A] = Ready(()).map { _ => f() }

  /** Program which (when interpreted) creates a program and runs it. */
  def defer[A](program: () => Program4[A]) = Ready(()).flatMap { _ => program() }

  /** Primitive [[Program4]] interpreter. */
  def run1[A](program: Program4[A]): A =
    program match {
      case Ready(a) =>
        a

      case Map(program, f) =>
        val a1 = run1(program) // not a tail call
        f(a1)

      case FlatMap(program, f) =>
        val a1 = run1(program) // not a tail call
        val program1: Program4[A] = f(a1)
        run1(program1)
    }

  /** Helper cases for [[run2]]. */
  sealed trait Todo[B]
  object Todo:
    case class More[A, B](program: Program4[A], todo: A => Todo[B]) extends Todo[B]
    case class Done[B](b: B) extends Todo[B]

  /** Tail recursive helper for [[run2]]. */
  @tailrec
  def loop2[A, B](program: Program4[A], todo: A => Todo[B]): B =
    import Todo._
    program match {
      case Ready(a) =>
        todo(a) match {
          case Done(b)             => b
          case More(program, todo) => loop2(program, todo)
        }

      case Map(program, f) =>
        loop2(program, x => todo(f(x)))

      case FlatMap(program, f) =>
        loop2(
          program,
          x => More(f(x), todo),
        )
    }

  /** Better interpreter of [[Program4]] – tail recursive version of [[run1]]. */
  def run2[A](program: Program4[A]): A =
    loop2(program, a => Todo.Done(a))

  /** Formal rewrite [[run2]], this time using [[Ready]] for [[Todo.Done]] and [[FlatMap]]
    * for [[Todo.More]].
    */
  @tailrec
  def run[A](program: Program4[A]): A =
    program match {
      case Ready(a) =>
        a

      case Map(program, f) =>
        val program1 = program.flatMap { a => Ready(f(a)) }
        run(program1)

      case FlatMap(program, f) =>
        program match {
          case Ready(a) =>
            run(f(a))
          case Map(program1, g) =>
            run(FlatMap(program1, a1 => f(g(a1))))
          case FlatMap(program1, g) =>
            run(FlatMap(program1, a1 => FlatMap(g(a1), f)))
        }
    }

  /** Computes the length of (3n+1)-sequence for given `n`. */
  def collatz(n: BigInt): Program4[BigInt] =
    println(s"collatz($n) [${Thread.currentThread.getStackTrace.size}]")
    defer { () =>
      println(s"deferred collatz($n) [${Thread.currentThread.getStackTrace.size}]")
      if (n == 1) ready(0)
      else {
        if (n % 2 == 0) collatz(n / 2).map(_ + 1)
        else collatz(3 * n + 1).map(_ + 1)
      }
    }

  def printResult(n: BigInt) = suspend { () => println(s"Result: $n") }

  @main def entry41(): Unit =
    run1(collatz(18).flatMap(printResult))

  @main def entry42(): Unit =
    run2(collatz(18).flatMap(printResult))

  @main def entry43(): Unit =
    run(collatz(18).flatMap(printResult))
