package au.id.tmm.citizenshipregisterscraper.scraping

import java.util.concurrent.{CancellationException, CompletableFuture}

import cats.effect.IO

package object aws {

  private[scraping] def toIO[A](completableFuture: CompletableFuture[A]): IO[A] =
    IO.cancelable[A] { cb: (Either[Throwable, A] => Unit) =>
      completableFuture.handle[Unit] { (a: A, e: Throwable) =>
        (a, e) match {
          case (null, e: CancellationException) => ()
          case (null, e)                        => cb(Left(e))
          case (a, _)                     => cb(Right(a))
        }
      }

      IO(completableFuture.cancel(true))
    }

}
