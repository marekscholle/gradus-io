package com.marekscholle.gradus

trait Program3[A]:
  def execute(): A

  def map[B](f: A => B): Program3[B] =
    flatMap { a => Program3.pure(f(a)) }

  def flatMap[B](f: A => Program3[B]): Program3[B] =
    Program3.flatMap(this, f)

object Program3:
  def pure[A](a: A): Program3[A] =
    new Program3[A]:
      def execute(): A = a

  def flatMap[A, B](
      program: Program3[A],
      f: A => Program3[B],
  ): Program3[B] =
    new Program3[B]:
      def execute(): B =
        val a = program.execute()
        val program1 = f(a)
        program1.execute()

  def flatten[A](program: Program2[Program2[A]]): Program2[A] =
    program.flatMap(identity)
