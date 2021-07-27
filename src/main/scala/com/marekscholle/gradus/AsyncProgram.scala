package com.marekscholle.gradus

import java.net.http.HttpRequest
import java.net.URI
import java.time.Duration
import java.net.http.HttpClient
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.HttpResponse
import cats.effect.kernel.syntax.async
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingDeque
import scala.annotation.tailrec

sealed trait AsyncProgram[A]:
  def map[B](f: A => B): AsyncProgram[B] =
    flatMap { a => AsyncProgram.Pure(f(a)) }

  def flatMap[B](f: A => AsyncProgram[B]): AsyncProgram[B] =
    AsyncProgram.FlatMap(this, f)

object AsyncProgram:
  import com.marekscholle.gradus.{AsyncProgram => Program}

  private case class Pure[A](value: A) extends Program[A]
  private case class FlatMap[A, B](
      program: Program[A],
      f: A => Program[B],
  ) extends Program[B]
  private case class Async[A](
      k: (A => Unit) => Unit,
  ) extends Program[A]

  def pure[A](a: A): AsyncProgram[A] = Pure(a)

  def delay[A](f: () => A): Program[A] =
    pure(()).map(_ => f())

  def defer[A](f: () => Program[A]): Program[A] =
    pure(()).flatMap(_ => f())

  def async[A](k: (A => Unit) => Unit): Program[A] =
    Async(k)

  def singleHttpRequest(uri: String): Program[Int] =
    async[Int] { cb =>
      val client = HttpClient.newBuilder().build()
      val request = HttpRequest
        .newBuilder()
        .uri(URI.create(uri))
        .timeout(Duration.ofSeconds(10))
        .GET()
        .build

      client
        .sendAsync(request, BodyHandlers.ofString)
        .thenAccept { response =>
          cb(response.statusCode)
        }
    }

  def naiveFromFuture[A](
      future: Future[A],
  )(using ec: ExecutionContext): Program[A] =
    async { cb =>
      future.onComplete {
        case Success(a) => cb(a)
        case Failure(e) => ???
      }
    }

  def fromFuture[A](
      future: Program[Future[A]],
  )(using ec: ExecutionContext): Program[A] =
    future.flatMap(naiveFromFuture)

  def run[A](program: Program[A], enqueue: Program[A] => Unit): Unit =
    program match
      case Pure(a) =>
        ()

      case FlatMap(program1, f) =>
        program1 match
          case Pure(b) =>
            val program2 = f(b)
            enqueue(program2)

          case FlatMap(program2, g) =>
            val program3 = FlatMap(program2, c => FlatMap(g(c), f))
            enqueue(program3)

          case Async(k) =>
            k { b => enqueue(f(b)) }

      case Async(k) =>
        k { a => enqueue(pure(a)) }

  def run(programs: Seq[Program[Unit]]): Nothing =
    val queue: BlockingQueue[Program[Unit]] = LinkedBlockingDeque()
    import scala.jdk.CollectionConverters._
    queue.addAll(programs.asJava)

    @tailrec
    def loop(): Nothing =
      val program = queue.take()
      run(program, queue.add)
      loop()

    loop()

  @main def AsyncProgram_main(): Unit =
    def fetch(n: Int, uri: String): Program[Unit] =
      if (n == 0)
        pure(())
      else
        singleHttpRequest(uri).flatMap { code =>
          println(s"fetched $uri: $code")
          fetch(n - 1, uri)
        }

    val programs = Seq(
      fetch(5, "https://example.com"),
      fetch(5, "https://google.com"),
    )
    run(programs)
