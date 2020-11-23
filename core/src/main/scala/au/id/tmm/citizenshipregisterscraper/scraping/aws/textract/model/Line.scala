package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import scala.collection.immutable.ArraySeq

final case class Line(
  id: BlockId,
  page: PageNumber,
  geometry: Geometry,
  text: String,
  words: ArraySeq[Word],
)
