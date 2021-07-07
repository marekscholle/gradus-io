package com.marekscholle.gradus

import scala.annotation.tailrec

/** Similar to [[Program2]] or [[Program3]], but leaves the program execution to an
  * interpreter; relieves [[Program]] from obligation to be able to execute itself.
  */
sealed trait Program[A]:
  def map[B](f: A => B): Program[B] = Program.Map(this, f)
  def flatMap[B](f: A => Program[B]): Program[B] = Program.FlatMap(this, f)

object Program:
  // +--------------------------------+
  // + Implementations of `Program4`. |
  // +--------------------------------+

  /** Program which (when interpreted) returns an already existing value. */
  private case class Ready[A](a: A) extends Program[A]

  /** Program which (when interpreted) runs the `program`, applies `f` on its result (i.e.
    * re-maps the result of `program`), and returns its result.
    */
  private case class Map[A, B](
      program: Program[A],
      f: A => B,
  ) extends Program[B]

  /** Program which (when interpreted) runs the `program`, applies `f` on its result to get
    * an another program, runs it and returns its result.
    */
  private case class FlatMap[A, B](
      program: Program[A],
      f: A => Program[B],
  ) extends Program[B]

  // +---------------------------+
  // | Factories for `Program4`. |
  // +---------------------------+

  /** Program which (when interpreted) returns an already existing value. */
  def ready[A](a: A): Program[A] = Ready(a)

  /** Program which (when interpreted) runs the function `f` and returns its result. */
  def suspend[A](f: () => A): Program[A] = ready(()).map { _ => f() }

  /** Program which (when interpreted) creates a program and runs it. */
  def defer[A](program: () => Program[A]) = ready(()).flatMap { _ => program() }

  // +------------------------------+
  // | Interpreters for `Program4`. |
  // +------------------------------+

  /** Naive [[Program4]] interpreter. */
  def run1[A](program: Program[A]): A =
    program match
      case Ready(a) =>
        a

      case Map(program, f) =>
        val a1 = run1(program) // not a tail call
        f(a1)

      case FlatMap(program, f) =>
        val a1 = run1(program) // not a tail call
        val program1: Program[A] = f(a1)
        run1(program1)

  /** Helper cases for [[run2]]. */
  sealed trait Todo[B]
  object Todo:
    case class More[A, B](program: Program[A], todo: A => Todo[B]) extends Todo[B]
    case class Done[B](b: B) extends Todo[B]

  /** Tail recursive helper for [[run2]]. */
  @tailrec
  def loop2[A, B](program: Program[A], todo: A => Todo[B]): B =
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

  /** Better interpreter of [[Program]] – tail recursive version of [[run1]]. */
  def run2[A](program: Program[A]): A =
    loop2(program, a => Todo.Done(a))

  /** Formal rewrite [[run2]], using [[Ready]] for [[Todo.Done]] and [[FlatMap]] for
    * [[Todo.More]].
    */
  @tailrec
  def run[A](program: Program[A]): A =
    program match {
      case Ready(a) =>
        a

      case FlatMap(program, f) =>
        program match
          case Ready(a) =>
            run(f(a))
          case Map(program1, g) =>
            run(FlatMap(program1, a1 => f(g(a1))))
          case FlatMap(program1, g) =>
            run(FlatMap(program1, a1 => FlatMap(g(a1), f)))
    }

// +--------------------+
// | Example `Program` |
// +--------------------+

/** Computes the length of (3n+1)-sequence for given `n`. */
def collatz(n: BigInt): Program[BigInt] =
  def stackDepth = Thread.currentThread.getStackTrace.size

  println(s"collatz($n) [${stackDepth}]")
  Program.defer { () =>
    println(s"deferred collatz($n) [${stackDepth}]")
    if (n == 1)
      Program.ready(0)
    else {
      if (n % 2 == 0) collatz(n / 2).map(_ + 1)
      else collatz(3 * n + 1).map(_ + 1)
    }
  }

/** Prints result `n` to console. */
def printResult(n: BigInt) = Program.suspend { () => println(s"Result: $n") }

@main def entry41(): Unit =
  Program.run1(collatz(6).flatMap(printResult))

@main def entry42(): Unit =
  Program.run2(collatz(6).flatMap(printResult))

@main def entry43(): Unit =
  Program.run(collatz(18).flatMap(printResult))
