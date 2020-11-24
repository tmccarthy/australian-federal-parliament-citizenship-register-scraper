package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}

final case class PageNumber private (asInt: Int) extends AnyVal

object PageNumber {
  def apply(asInt: Int): ExceptionOr[PageNumber] =
    asInt match {
      case p if p >= 0 => Right(new PageNumber(p))
      case badPage     => Left(GenericException(s"Bad confidence value $badPage"))
    }

  implicit val ordering: Ordering[PageNumber] = Ordering.by(_.asInt)
}
