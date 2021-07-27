package com.marekscholle.gradus

import scala.annotation.tailrec

sealed trait Program[A]:
  def map[B](f: A => B): Program[B] =
    flatMap { a => Program.pure(f(a)) }

  def flatMap[B](f: A => Program[B]): Program[B] =
    Program.FlatMap(this, f)

object Program:
  case class Pure[A](value: A) extends Program[A]
  case class FlatMap[A, B](
      program: Program[A],
      f: A => Program[B],
  ) extends Program[B]

  def pure[A](a: A): Program[A] = Pure(a)

  def delay[A](f: () => A): Program[A] =
    pure(()).map(_ => f())

  def defer[A](f: () => Program[A]): Program[A] =
    pure(()).flatMap(_ => f())

  def run1[A](program: Program[A]): A =
    program match
      case Pure(a) =>
        a

      case FlatMap(program1, f) =>
        val b = run1(program1)
        val program2 = f(b)
        run1(program2)

  def collatz(n: BigInt): Program[BigInt] =
    defer { () =>
      if (n == 1)
        pure(0)
      else if (n % 2 == 0) collatz(n / 2).map(_ + 1)
      else collatz(3 * n + 1).map(_ + 1)
    }

  def printLength(n: BigInt): Program[Unit] =
    delay { () =>
      println(s"Length: $n")
    }

  sealed trait Todo[A]
  object Todo:
    case class Done[A](value: A) extends Todo[A]
    case class More[A, B](
        program: Program[A],
        todo: A => Todo[B],
    ) extends Todo[B]

  @tailrec
  def loop2[A, B](program: Program[A], todo: A => Todo[B]): B =
    import Todo._
    program match
      case Pure(a) =>
        todo(a) match {
          case Done(b)              => b
          case More(program1, todo) => loop2(program1, todo)
        }

      case FlatMap(program, f) =>
        loop2(
          program,
          x => More(f(x), todo),
        )

  def run2[A](program: Program[A]): A =
    loop2(program, a => Todo.Done(a))

  object OverflowOddEven:
    def even[A](list: List[A]): Boolean =
      list match
        case head :: tail => odd(tail)
        case Nil          => true

    def odd[A](list: List[A]): Boolean =
      list match
        case head :: tail => even(tail)
        case Nil          => false

  object ProgramOddEven:
    def even[A](list: List[A]): Program[Boolean] = defer { () =>
      list match
        case head :: tail => odd(tail)
        case Nil          => pure(true)
    }

    def odd[A](list: List[A]): Program[Boolean] = defer { () =>
      list match
        case head :: tail => even(tail)
        case Nil          => pure(false)
    }

  @tailrec
  def run[A](program: Program[A]): A =
    program match
      case Pure(a) =>
        a

      case FlatMap(program1, f) =>
        program1 match
          case Pure(b) =>
            val program2 = f(b)
            run(program2)

          case FlatMap(program2, g) =>
            val program3 = FlatMap(program2, c => FlatMap(g(c), f))
            run(program3)

  @main def Program_main(): Unit =
    import Utils._

    "collatz1" >>> {
      val program = collatz(6).flatMap(printLength)
      run1(program)
    }

    "collatz2" >>> {
      run2(collatz(6))
    }
