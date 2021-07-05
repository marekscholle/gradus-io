package com.marekscholle.gradus

import cats.instances.double

/** The same as [[Program2]], but [[map]] and [[flatMap]] implementations are delegated to
  * functions in companion object.
  */
trait Program3[A]:
  def execute(): A
  def map[B](f: A => B): Program3[B] = Program3.map(this, f)
  def flatMap[B](f: A => Program3[B]): Program3[B] = Program3.flatMap(this, f)

object Program3:
  /** Program which returns an already existing value. */
  def ready[A](a: A): Program3[A] =
    new Program3:
      def execute(): A = a

  /** The implementation of [[Program3.map]]. */
  def map[A, B](
      program: Program3[A],
      f: A => B,
  ): Program3[B] =
    new Program3:
      def execute(): B =
        val a = program.execute()
        f(a)

  /** The implementation of [[Program3.flatMap]]. */
  def flatMap[A, B](
      program: Program3[A],
      f: A => Program3[B],
  ): Program3[B] =
    new Program3:
      def execute(): B = f(program.execute()).execute()
