package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import scala.collection.immutable.ArraySeq

final case class Table(
  id: BlockId,
  page: PageNumber,
  geometry: Geometry,
  children: ArraySeq[Table.Cell],
)

object Table {

  final case class Cell(
    id: BlockId,
    page: PageNumber,
    geometry: Geometry,
    columnIndex: Int,
    columnSpan: Int,
    rowIndex: Int,
    rowSpan: Int,
    words: ArraySeq[Word],
  )

}