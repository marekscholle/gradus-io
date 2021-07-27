package com.marekscholle.gradus

trait Program1:
  def execute(): Unit

  def andThen(that: Program1): Program1 =
    val self = this
    new Program1:
      def execute(): Unit =
        self.execute()
        that.execute()

object Program1:
  val captureExample: Program1 =
    val a = 1
    val b = 2
    new Program1:
      def execute(): Unit =
        println(s"$a + $b = ${a + b}")

  val dummy =
    new Program1:
      def execute(): Unit = ()

  @main def Program1_main(): Unit =
    import Utils._

    "capture" >>> { captureExample.execute() }
    "dummy" >>> { dummy.execute() }
