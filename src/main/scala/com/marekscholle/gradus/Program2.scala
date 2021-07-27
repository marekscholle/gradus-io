package com.marekscholle.gradus

trait Program2[A]:
  def execute(): A

  def map[B](f: A => B): Program2[B] =
    val self = this
    new Program2[B]:
      def execute(): B =
        val a = self.execute()
        f(a)

  def flatMap[B](f: A => Program2[B]): Program2[B] =
    val self = this
    new Program2[B]:
      def execute(): B =
        val a = self.execute()
        val program1 = f(a)
        program1.execute()

object Program2:
  def flatten[A](program: Program2[Program2[A]]): Program2[A] =
    new Program2[A]:
      def execute(): A =
        val program1 = program.execute()
        program1.execute()

  def pure[A](a: A): Program2[A] =
    new Program2[A]:
      def execute(): A = a

  def collatz(n: BigInt): Program2[BigInt] =
    new Program2[BigInt]:
      def execute(): BigInt =
        if (n == 1) 0
        else
          val c =
            if (n % 2 == 0) collatz(n / 2)
            else collatz(3 * n + 1)
          c
            .map(_ + 1)
            .execute()

  def printLength(n: BigInt): Program2[Unit] =
    new Program2[Unit]:
      def execute(): Unit =
        println(s"Length: $n")

  def collatzAndPrint(n: BigInt): Program2[Unit] =
    collatz(n).flatMap(printLength)

  @main def Program2_main(): Unit =
    import Utils._
    "collatz" >>> { collatzAndPrint(6).execute() }
