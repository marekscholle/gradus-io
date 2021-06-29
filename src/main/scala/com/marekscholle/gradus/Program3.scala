package com.marekscholle.gradus

import org.slf4j.LoggerFactory
import cats.instances.double

trait Program3[A]:
  def execute(): A
  def map[B](f: A => B): Program3[B] = Program3.map(this, f)
  def flatMap[B](f: A => Program3[B]): Program3[B] = Program3.flatMap(this, f)

object Program3:
  /** Program which returns already existing value `a`. */
  def ready[A](a: A): Program3[A] =
    new Program3:
      def execute(): A = a

  /** The implementation of [[Program2.map]], as a named class. */
  def map[A, B](
      program: Program3[A],
      f: A => B,
  ): Program3[B] =
    new Program3:
      def execute(): B = f(program.execute())

  /** The implementation of [[Program2.flatMap]], as a named class. */
  def flatMap[A, B](
      program: Program3[A],
      f: A => Program3[B],
  ): Program3[B] =
    new Program3:
      def execute(): B = f(program.execute()).execute()
