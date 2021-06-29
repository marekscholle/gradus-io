package com.marekscholle.gradus

/** First version of code representation of a _program_. */
trait Program1:
  /** Executes arbitrary piece of code and returns "nothing".
    *
    * The same as Java's [[java.lang.Runnable]]:
    * (https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/lang/Runnable.html).
    */
  def execute(): Unit

  /** Program which first executes `this` program and then `that` program. */
  def andThen(that: Program1): Program1 =
    val self = this
    new Program1:
      def execute(): Unit =
        self.execute()
        that.execute()

object Program1:
  /** Prints progress in computation of (3n + 1)-sequence.
    *
    * https://en.wikipedia.org/wiki/Collatz_conjecture
    */
  def collatz(n: BigInt): Program1 =
    println(s"creating program collatz($n)")
    new Program1:
      def execute(): Unit =
        println(s"collatz($n): execute()")
        if (n == 1) ()
        else
          val c =
            if (n % 2 == 0) collatz(n / 2)
            else collatz(3 * n + 1)
          c.execute()

  /** Executes given program. */
  def run(program1: Program1): Unit =
    program1.execute()

  @main def entry11(): Unit =
    run(collatz(18))

  @main def entry12(): Unit =
    run(collatz(3) andThen collatz(7))
