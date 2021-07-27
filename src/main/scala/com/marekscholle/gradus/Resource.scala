package com.marekscholle.gradus

object Resource:
  def programUsingResource[R, A](
      acquire: IO[R],
      use: R => IO[A],
      release: R => IO[Unit],
  ): IO[A] =
    acquire
      .flatMap { r =>
        use(r).attempt
          .flatMap {
            case Left(e) =>
              release(r).attempt
                *> IO.raiseError(e)
            case Right(a) =>
              release(r)
                .as(a)
          }
      }
