package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Page.Child
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.ResultNavigator.{NoParentFor, NotFoundInResults, Parent, Siblings}
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException, ProductException}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

final class ResultNavigator private(
  analysisResult: AnalysisResult,
  atomicBlockParents: collection.Map[AtomicBlock, Parent.ForAtomicBlock],
  cellParents: collection.Map[Table.Cell, Table],
  tableParents: collection.Map[Table, Page],
  lineParents: collection.Map[Line, Page],

  tableCellLookup: collection.Map[Table, Map[(Int, Int), Table.Cell]],
) {

  def parentOf(atomicBlock: AtomicBlock): ExceptionOr[Parent.ForAtomicBlock] =
    atomicBlockParents.get(atomicBlock).toRight(NoParentFor(atomicBlock))

  def parentOf(cell: Table.Cell): ExceptionOr[Table] =
    cellParents.get(cell).toRight(NoParentFor(cell))

  def parentOf(table: Table): ExceptionOr[Page] =
    tableParents.get(table).toRight(NoParentFor(table))

  def parentOf(line: Line): ExceptionOr[Page] =
    lineParents.get(line).toRight(NoParentFor(line))

  def siblingsOf(atomicBlock: AtomicBlock): ExceptionOr[ArraySeq[AtomicBlock]] =
    parentOf(atomicBlock).map {
      case Parent.ForAtomicBlock.OfLine(line) => line.children.filter(b => b != atomicBlock)
      case Parent.ForAtomicBlock.OfCell(cell) => cell.children.filter(b => b != atomicBlock)
    }

  def siblingsOf(cell: Table.Cell): ExceptionOr[ArraySeq[Table.Cell]] =
    parentOf(cell).map { table =>
      table.children.filter(c => c != cell)
    }

  def siblingsOf(table: Table): ExceptionOr[ArraySeq[Siblings.UnderPage]] =
    parentOf(table).map { page =>
      page.children.collect {
        case Page.Child.OfLine(line) => Siblings.UnderPage.OfLine(line)
        case Page.Child.OfTable(siblingTable) if siblingTable != table => Siblings.UnderPage.OfTable(siblingTable)
      }
    }

  def siblingsOf(line: Line): ExceptionOr[ArraySeq[Siblings.UnderPage]] =
    parentOf(line).map { page =>
      page.children.collect {
        case Page.Child.OfLine(siblingLine) if siblingLine != line => Siblings.UnderPage.OfLine(siblingLine)
        case Page.Child.OfTable(table) => Siblings.UnderPage.OfTable(table)
      }
    }

  def siblingsOf(page: Page): ExceptionOr[ArraySeq[Page]] = {
    if (analysisResult.pages.contains(page)) {
      Right(analysisResult.pages.filter(p => p != page))
    } else {
      Left(NotFoundInResults(page))
    }
  }

  def findCell(table: Table, columnIndex: Int, rowIndex: Int): ExceptionOr[Table.Cell] =
    for {
      cellLookup <- tableCellLookup.get(table)
      .toRight(NotFoundInResults(table))

      cell <- cellLookup.get((columnIndex, rowIndex))
      .toRight(GenericException(s"Cell $columnIndex, $rowIndex not found"))
    } yield cell

}

object ResultNavigator {

  final case class NoParentFor(block: Block) extends ProductException
  final case class NotFoundInResults(block: Block) extends ProductException

  object Parent {

    sealed trait ForAtomicBlock

    object ForAtomicBlock {
      final case class OfLine(line: Line) extends Parent.ForAtomicBlock
      final case class OfCell(cell: Table.Cell) extends Parent.ForAtomicBlock
    }

  }

  object Siblings {
    sealed trait UnderPage

    object UnderPage {
      final case class OfLine(line: Line) extends Siblings.UnderPage
      final case class OfTable(table: Table) extends Siblings.UnderPage
    }
  }

  /**
   * Returns children of the given block, excluding any child key-value sets.
   */
  def enclosedChildrenOf(block: Block): ArraySeq[Block] = block match {
    case _: AtomicBlock => ArraySeq.empty
    case line: Line => line.children
    case page: Page => page.children.collect {
      case Child.OfLine(line) => line
      case Child.OfTable(table) => table
    }
    case table: Table => table.children
    case cell: Table.Cell => cell.children
    case key: KeyValueSet.Key => key.children
    case value: KeyValueSet.Value => value.children
  }

  def apply(analysisResult: AnalysisResult): ResultNavigator = {
    val atomicBlockParents: mutable.Map[AtomicBlock, Parent.ForAtomicBlock] = mutable.Map()
    val cellParents: mutable.Map[Table.Cell, Table] = mutable.Map()
    val tableParents: mutable.Map[Table, Page] = mutable.Map()
    val lineParents: mutable.Map[Line, Page] = mutable.Map()

    val tableCellLookup: mutable.Map[Table, Map[(Int, Int), Table.Cell]] = mutable.Map()

    analysisResult.pages.foreach { page =>
      page.children.foreach {
        case Child.OfLine(line) => {
          val lineAsAtomicBlockParent = Parent.ForAtomicBlock.OfLine(line)
          lineParents.put(line, page)
          line.children.foreach { atomicBlock =>
            atomicBlockParents.put(atomicBlock, lineAsAtomicBlockParent)
          }
        }
        case Child.OfTable(table) => {
          tableParents.put(table, page)
          table.children.foreach { cell =>
            val cellAsAtomicBlockParent = Parent.ForAtomicBlock.OfCell(cell)
            cellParents.put(cell, table)
            cell.children.foreach { atomicBlock =>
              atomicBlockParents.put(atomicBlock, cellAsAtomicBlockParent)
            }
          }
        }
        case Child.OfKeyValueSet(keyValueSet) => ()
      }
    }

    new ResultNavigator(
      analysisResult, atomicBlockParents, cellParents, tableParents, lineParents, tableCellLookup,
    )
  }

}
