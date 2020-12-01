package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract

import java.util.UUID

import au.id.tmm.utilities.errors.ExceptionOr

final case class TextractJobId(asUUID: UUID) extends AnyVal

object TextractJobId {
  def fromString(string: String): ExceptionOr[TextractJobId] =
    ExceptionOr.catchIn(TextractJobId(UUID.fromString(string)))
}
