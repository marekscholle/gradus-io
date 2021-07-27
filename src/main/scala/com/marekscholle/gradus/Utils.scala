package com.marekscholle.gradus

object Utils:
  extension (head: String)
    def >>>[A](thunk: => A): Unit =
      println(s"$head\n>>>")
      val res = thunk
      println(s"===\nResult: $res\n<<<\n")
