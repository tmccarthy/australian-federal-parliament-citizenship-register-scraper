package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import scala.collection.immutable.ArraySeq

final case class KeyValueSet(
  key: KeyValueSet.Key,
  value: KeyValueSet.Value,
)

object KeyValueSet {
  final case class Key(
    id: BlockId,
    page: PageNumber,
    geometry: Geometry,
    words: ArraySeq[Word],
  )

  final case class Value(
    id: BlockId,
    page: PageNumber,
    geometry: Geometry,
    words: ArraySeq[Word],
  )
}
