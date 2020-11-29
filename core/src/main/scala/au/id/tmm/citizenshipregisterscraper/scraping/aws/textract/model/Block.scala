package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import scala.collection.immutable.ArraySeq

sealed trait HasBlockId {
  val id: BlockId
}

sealed trait Block extends HasBlockId {
  val pageNumber: PageNumber
  val geometry: Geometry
}

final case class Line(
  id: BlockId,
  pageNumber: PageNumber,
  geometry: Geometry,
  text: String,
  children: ArraySeq[AtomBlock],
) extends Block

final case class Page(
  id: BlockId,
  pageNumber: PageNumber,
  geometry: Geometry,
  children: ArraySeq[Page.Child],
) extends Block

object Page {
  sealed trait Child

  object Child {
    final case class OfLine(line: Line)                      extends Child
    final case class OfTable(table: Table)                   extends Child
    final case class OfKeyValueSet(keyValueSet: KeyValueSet) extends Child
  }

  implicit val ordering: Ordering[Page] = Ordering.by(_.pageNumber)
}

final case class SelectionElement(
  id: BlockId,
  pageNumber: PageNumber,
  geometry: Geometry,
  status: SelectionElement.Status,
) extends Block

object SelectionElement {
  sealed abstract class Status(val isSelected: Boolean)

  object Status {
    case object Selected    extends Status(isSelected = true)
    case object NotSelected extends Status(isSelected = false)
  }
}

final case class Table(
  id: BlockId,
  pageNumber: PageNumber,
  geometry: Geometry,
  children: ArraySeq[Table.Cell],
) extends Block {
  def rows: ArraySeq[ArraySeq[Table.Cell]] =
    children
      .sortBy(c => (c.rowIndex, c.columnIndex))
      .groupBy(c => c.rowIndex)
      .to(ArraySeq)
      .sortBy(_._1)
      .map(_._2)
}

object Table {

  final case class Cell(
    id: BlockId,
    pageNumber: PageNumber,
    geometry: Geometry,
    columnIndex: Int,
    columnSpan: Int,
    rowIndex: Int,
    rowSpan: Int,
    children: ArraySeq[AtomBlock],
  ) extends Block

}

final case class Word(
  id: BlockId,
  pageNumber: PageNumber,
  geometry: Geometry,
  text: String,
  confidence: Confidence,
  textType: Word.TextType,
) extends Block

object Word {
  sealed trait TextType

  object TextType {
    case object Printed     extends TextType
    case object Handwriting extends TextType
  }
}

final case class KeyValueSet(
  key: KeyValueSet.Key,
  value: KeyValueSet.Value,
)

object KeyValueSet {
  final case class Key(
    id: BlockId,
    pageNumber: PageNumber,
    geometry: Geometry,
    children: ArraySeq[AtomBlock],
  ) extends Block

  final case class Value(
    id: BlockId,
    pageNumber: PageNumber,
    geometry: Geometry,
    children: ArraySeq[AtomBlock],
  ) extends Block
}

sealed trait AtomBlock // TODO this is not a good name

object AtomBlock {
  final case class OfWord(word: Word)                                     extends AtomBlock
  final case class OfSelectionElement(selectionElement: SelectionElement) extends AtomBlock
}
