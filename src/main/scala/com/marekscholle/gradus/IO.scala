package com.marekscholle.gradus

import java.util.concurrent.LinkedBlockingDeque
import scala.annotation.tailrec
import java.util.concurrent.LinkedBlockingQueue
import scala.util.{Failure, Success, Try}
import cats.Eval.Leaf
import java.time.Duration
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/** Program which execution results in value of type `A` or fails with exception.
  *
  * We call a program which execution results in a value of type `A` as _normal_, a program
  * which execution ends with exception as _failed_ or _raised_.
  *
  * WARNING: Execution of program may involve side effects, so the same program may once end
  * normally and once fail, so "normal" or "raised" is not an inherent property of program,
  * but relates to its execution. Nevertheless, we colloquially say "failed program" for
  * "program we executed and got failure". Is is necessary to keep this distinction in mind.
  *
  * Production ready IO would provide:
  *   - more control of execution fairness,
  *   - support for work with time,
  *   - possibility to opt-in to features,
  *   - execution on multiple thread,
  *   - support for working with blocking operations,
  *   - fibers (green threads),
  *   - and more.
  */
sealed trait IO[+A]:
  /** Applies a transformation to the result of `this`.
    *
    * No-op if `this` fails, raises if `f` threw.
    */
  def map[B](f: A => B): IO[B] =
    flatMap { a => IO.Pure(f(a)) }

  /** Convenience [[map]] variant which ignores the result of `this`. */
  def as[B](b: B): IO[B] =
    map { _ => b }

  /** Basic building block for sequential execution of programs: uses the result of `this`
    * to create a new program which is executed next.
    *
    * No-op if `this` fails, raises if `f` throws.
    */
  def flatMap[B](f: A => IO[B]): IO[B] =
    IO.FlatMap(this, f, IO.Error(_))

  /** Convenience [[flatMap]] variant which ignores the result of `this`. */
  def *>[B](that: IO[B]): IO[B] =
    flatMap { _ => that }

  /** Recovers failure of `this` back to normal. IO variant of try-catch block.
    *
    * The failure is communicated via result as `Either[Throwable, A]`. An `attempt`ed IO
    * never fails.
    */
  def attempt: IO[Either[Throwable, A]] =
    IO.FlatMap(this, a => IO.Pure(Right(a)), e => IO.Pure(Left(e)))

  /** Program which executes `this` and `that` in "parallel".
    *
    * Note that if the both programs are fully synchronous, it is left to interpreter
    * implementation if they are executed truly in parallel. At least it should be true that
    * if one of program can't progress due to async step(s), the other should execute in the
    * meantime (up to multithreading races).
    *
    * Building block for parallel execution of sequences of programs.
    */
  def par[B](that: IO[B]): IO[(A, B)] =
    IO.Par(this, that, (a, b) => IO.Pure((a, b)))

  /** Yields from execution of `this`.
    *
    * Prefixed with `exec` because `yield` is a keyword.
    */
  def execYield: IO[A] =
    IO.Yield(this)

object IO:
  private case class Pure[A](value: A) extends IO[A]
  private case class Error(cause: Throwable) extends IO[Nothing]
  private case class Yield[A](io: IO[A]) extends IO[A]
  private case class FlatMap[A, B](
      io: IO[A],
      f: A => IO[B], // may throw
      // onError semantics: to be used every time we could not obtain `f(a)`
      // to be executed next either because
      // - we could not obtain `a`
      // - `io` resulted in `Error`
      recover: Throwable => IO[B], // internal only, never throws
  ) extends IO[B]
  private case class Async[A](k: (Either[Throwable, A] => Unit) => Unit) extends IO[A]
  private case class Par[A, B, C](
      io1: IO[A],
      io2: IO[B],
      combine: (A, B) => IO[C], // internal only, never throws
  ) extends IO[C]

  // Client-facing factories
  def pure[A](a: A): IO[A] = Pure(a)
  val unit = pure(())
  def raiseError[A](cause: Throwable): IO[A] = Error(cause)
  def delay[A](f: () => A): IO[A] = unit.map { _ => f() }
  def defer[A](f: () => IO[A]): IO[A] = unit.flatMap { _ => f() }
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): IO[A] = Async(k)

  /** Primitive single-threaded interpreter which does as much as it can synchronously
    * (until explicit yield or async operation).
    *
    * If you need a CPU heavy computations, you should run it on dedicated thread pool
    * (executor), put your work there with [[async]]. To break longer synchronous IOs into
    * smaller pieces and achieve fairness, use [[IO.execYield]].
    */
  def run[A](ioa: IO[A]): A =
    case object Continue

    // internal job queue
    // we append to this queue from async callbacks, that's why it needs to be
    // protected against MT races
    val jobQueue = new LinkedBlockingQueue[IO[A | Continue.type]]()
    def submit(io: IO[A | Continue.type]): Unit = jobQueue.add(io)

    /** Interprets provided IO until it can't make synchronous progress or encounters
      * explicit yield ([[Yield]]).
      *
      * It returns either:
      *   - result in form of [[Pure]] or [[Error]],
      *   - or [[Continue]] if it needed to submit a job to queue and can't make synchronous
      *     progress.
      */
    @tailrec
    def _run(io: IO[A | Continue.type]): Pure[A] | Error | Continue.type =
      io match
        case Pure(a) =>
          a match
            case Continue => Continue
            case a        =>
              // unfortunately Scala compiler can't prove `a` is an `A` 😔
              Pure(a.asInstanceOf[A])

        case Error(e) => Error(e)

        case Yield(io) =>
          submit(io)
          Continue

        case FlatMap(io, f, recover) =>
          io match
            case Pure(a) =>
              Try(f(a)) match
                case Failure(e)   => _run(recover(e))
                case Success(iob) => _run(iob)

            case Error(e) =>
              _run(recover(e))

            case Yield(io) =>
              _run(Yield(FlatMap(io, f, recover)))

            case FlatMap(io, g, recover1) =>
              _run(
                FlatMap(
                  io,
                  a =>
                    Try(g(a)) match
                      case Failure(e)   => recover(e)
                      case Success(iob) => FlatMap(iob, f, recover)
                  ,
                  e => FlatMap(recover1(e), f, recover),
                ),
              )

            case Async(k) =>
              k {
                case Left(e) => submit(recover(e))
                case Right(a) =>
                  Try(f(a)) match
                    case Failure(e)   => submit(recover(e))
                    case Success(iob) => submit(iob)
              }
              Continue

            case Par(io1, io2, g) =>
              _run(
                Par(
                  io1,
                  io2,
                  (a, b) => FlatMap(g(a, b), f, recover),
                ),
              )

        case Async(k) =>
          k {
            case Left(e)  => submit(Error(e))
            case Right(a) => submit(Pure(a))
          }
          Continue

        case Par(io1, io2, combine) =>
          // `process` is here to help us name type parameters of `io1` and `io2`
          def process[B, C](
              io1: IO[B],
              io2: IO[C],
              combine: (B, C) => IO[A | Continue.type],
          ): Continue.type =
            // allocate mutable boxes to store the results
            var _1: Option[Either[Throwable, B]] = None
            var _2: Option[Either[Throwable, C]] = None

            val ioc1 = io1.attempt.map { b =>
              _1 = Some(b)
              b match
                case Left(e) =>
                  _2 match
                    case None | Some(Right(_)) => submit(Error(e))
                    case Some(Left(_))         => // already submitted error
                case Right(b) =>
                  _2 match
                    case None           => // waiting for _2
                    case Some(Left(_))  => // already submitted error
                    case Some(Right(c)) => submit(combine(b, c))

              Continue
            }
            val ioc2 = io2.attempt.map { c =>
              _2 = Some(c)
              c match
                case Left(e) =>
                  _1 match
                    case None | Some(Right(_)) => submit(Error(e))
                    case Some(Left(_))         => // already submitted error
                case Right(c) =>
                  _1 match
                    case None           => // waiting for _1
                    case Some(Left(_))  => // already submitted error
                    case Some(Right(b)) => submit(combine(b, c))

              Continue
            }

            submit(ioc1)
            submit(ioc2)
            Continue

          process(io1, io2, combine)

    /** Runner of job queue. */
    @tailrec
    def loop(io: IO[A | Continue.type]): A =
      _run(io) match
        case Continue => loop(jobQueue.take())
        case Error(e) => throw e
        case Pure(a)  => a

    loop(ioa)

  // TODO write some example program to show real-world IO

  val executorService = Executors.newSingleThreadScheduledExecutor

  def delay(duration: Duration): IO[Unit] =
    IO.async { cb =>
      val command: Runnable = () => cb(Right(()))
      executorService.schedule(
        command,
        duration.toMillis,
        TimeUnit.MILLISECONDS,
      )
    }
