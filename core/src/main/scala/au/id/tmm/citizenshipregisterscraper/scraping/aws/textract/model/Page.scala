package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import scala.collection.immutable.ArraySeq

final case class Page(
  id: BlockId,
  page: PageNumber,
  geometry: Geometry,
  children: ArraySeq[Page.Child],
)

object Page {
  sealed trait Child

  object Child {
    final case class OfLine(line: Line) extends Child
    final case class OfTable(table: Table) extends Child
    final case class OfKeyValueSet(keyValueSet: KeyValueSet) extends Child
  }

  implicit val ordering: Ordering[Page] = Ordering.by(_.page)
}
