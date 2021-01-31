package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.utilities.errors.ExceptionOr

trait SyntaxErrorContext[F[_]] {

  def lift[A](exceptionOrA: ExceptionOr[A]): F[A]

}

object SyntaxErrorContext {

  type Unsafe[A] = A

  implicit val forUnsafe: SyntaxErrorContext[Unsafe] = new SyntaxErrorContext[Unsafe] {
    override def lift[A](exceptionOrA: ExceptionOr[A]): Unsafe[A] =
      exceptionOrA match {
        case Right(a) => a
        case Left(e)  => throw e
      }
  }

  implicit val forExceptionOr: SyntaxErrorContext[ExceptionOr] = new SyntaxErrorContext[ExceptionOr] {
    override def lift[A](exceptionOrA: ExceptionOr[A]): ExceptionOr[A] = exceptionOrA
  }

}
