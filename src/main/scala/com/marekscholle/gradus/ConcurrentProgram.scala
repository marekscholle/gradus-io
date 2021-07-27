package com.marekscholle.gradus

import scala.collection.mutable
import scala.annotation.tailrec

object ConcurrentProgram:
  import Program._

  def run[A](program: Program[A]): Either[A, Program[A]] =
    program match
      case Pure(a) =>
        Left(a)

      case FlatMap(program1, f) =>
        program1 match
          case Pure(b) =>
            val program2 = f(b)
            Right(program2)

          case FlatMap(program2, g) =>
            val program3 = FlatMap(program2, c => FlatMap(g(c), f))
            Right(program3)

  def run(programs: Seq[Program[Unit]]): Unit =
    val queue = mutable.Queue[Program[Unit]](programs: _*)

    @tailrec
    def loop(): Unit =
      if (queue.isEmpty) ()
      else
        val program = queue.dequeue()
        run(program) match
          case Left(_) =>
            loop()
          case Right(next) =>
            queue += next
            loop()

    loop()

  def countDown(label: String, n: Int): Program[Unit] =
    if (n == 0)
      pure(())
    else
      defer { () =>
        println(s"$label (n = $n)")
        countDown(label, n - 1)
      }

  @main def ConcurrentProgram_main(): Unit =
    import Utils._

    "example" >>> {
      val programs =
        Seq(
          countDown("Hello", 3),
          countDown("¡Hola!", 2),
          countDown("Ahoj", 1),
        )
      run(programs)
    }
