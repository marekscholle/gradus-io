package com.marekscholle.gradus

import org.slf4j.LoggerFactory
import cats.instances.double

/** Second version of code representation of a _program_.
  *
  * Unlike [[Program1]], this allows the program to return a value.
  */
trait Program2[A]:
  /** Executes arbitrary piece of code and returns value of type `A`.
    *
    * The the same as Java's [[java.lang.Supplier]]:
    * https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/function/Supplier.html
    *
    * As with [[java.lnag.Supplier]], there is no requirement that a new or distinct result
    * be returned each time the supplier is invoked.
    */
  def execute(): A

  /** Program which executes `this` program, passes the result as argument to `f` and return
    * its result.
    */
  def map[B](f: A => B): Program2[B] =
    val self = this
    new Program2[B]:
      def execute(): B =
        val a = self.execute()
        f(a)

  /** Program which first executes `this` program and passes the result as argument to `f`
    * to obtain another program which is then executed and its result returned.
    */
  def flatMap[B](f: A => Program2[B]): Program2[B] =
    val self = this
    new Program2[B]:
      def execute(): B =
        val a = self.execute()
        f(a).execute()

object Program2:
  val logger = LoggerFactory.getLogger(getClass)

  /** Executes given program. */
  def run[A](program2: Program2[A]): Unit =
    program2.execute()

  /** Program which returns already existing value `a`. */
  def ready[A](a: A): Program2[A] =
    new Program2:
      def execute(): A = a

  /** Computes the length of (3n+1)-sequence for given `n`.
    *
    * https://en.wikipedia.org/wiki/Collatz_conjecture
    */
  def collatz(n: BigInt): Program2[BigInt] =
    logger.debug(s"new collatz($n)")
    def loop(n: BigInt, length: BigInt): Program2[BigInt] =
      logger.debug(s"new collatz.loop($n, $length)")
      new Program2:
        def execute(): BigInt =
          logger.debug(
            s"collatz.loop($n, $length).execute()" +
              s", stack depth: ${Thread.currentThread.getStackTrace.size}"
          )
          if (n == 1) length
          else
            val c =
              if (n % 2 == 0) loop(n / 2, length + 1)
              else loop(3 * n + 1, length + 1)
            c.execute()
    loop(n, length = 0)

  /** (Program which) Prints `a` to console with given `label`. */
  def print[A](label: String, a: A): Program2[Unit] =
    new Program2:
      def execute(): Unit =
        println(s"$label: $a")

  /** (Program which) Computes the lenght of (3n+1)-sequence for given `n` and [[print]]
    * &ZeroWidthSpace;s the result to console.
    */
  def collatzAndPrint(n: BigInt): Program2[Unit] =
    collatz(n).flatMap { len => print(s"length($n)", len) }

  @main def entry21(): Unit =
    run(collatzAndPrint(18))
