package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Page.Child
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.ResultNavigator.Parent.ForAtomicBlock
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.ResultNavigator.Syntax.BlockOps
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.ResultNavigator.{
  NoParentFor,
  NotFoundInResults,
  Parent,
  Siblings,
}
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException, ProductException}
import au.id.tmm.utilities.errors.syntax._

import scala.annotation.tailrec
import scala.collection.immutable.{ArraySeq, ListSet}
import scala.collection.mutable
import scala.reflect.ClassTag

final class ResultNavigator private (
  private val analysisResult: AnalysisResult,
  atomicBlockParents: collection.Map[AtomicBlock, Parent.ForAtomicBlock],
  cellParents: collection.Map[Table.Cell, Table],
  tableParents: collection.Map[Table, Page],
  lineParents: collection.Map[Line, Page],
  kvSetParents: collection.Map[KeyValueSet, Page],
  tableCellLookup: collection.Map[Table, Map[(Int, Int), Table.Cell]],
  kvSetsForKeys: collection.Map[KeyValueSet.Key, KeyValueSet],
  kvSetsForValues: collection.Map[KeyValueSet.Value, KeyValueSet],
) {

  def parentOf(atomicBlock: AtomicBlock): ExceptionOr[Parent.ForAtomicBlock] =
    atomicBlockParents.get(atomicBlock).toRight(NoParentFor(atomicBlock))

  def parentOf(cell: Table.Cell): ExceptionOr[Table] =
    cellParents.get(cell).toRight(NoParentFor(cell))

  def parentOf(table: Table): ExceptionOr[Page] =
    tableParents.get(table).toRight(NoParentFor(table))

  def parentOf(line: Line): ExceptionOr[Page] =
    lineParents.get(line).toRight(NoParentFor(line))

  def parentOf(keyValueSet: KeyValueSet): ExceptionOr[Page] =
    kvSetParents.get(keyValueSet).toRight(NoParentFor(keyValueSet.key))

  def parentOf(key: KeyValueSet.Key): ExceptionOr[Page] =
    kvSetFor(key).flatMap(parentOf)

  def parentOf(value: KeyValueSet.Value): ExceptionOr[Page] =
    kvSetFor(value).flatMap(parentOf)

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
        case Page.Child.OfLine(line)                                   => Siblings.UnderPage.OfLine(line)
        case Page.Child.OfTable(siblingTable) if siblingTable != table => Siblings.UnderPage.OfTable(siblingTable)
      }
    }

  def siblingsOf(line: Line): ExceptionOr[ArraySeq[Siblings.UnderPage]] =
    parentOf(line).map { page =>
      page.children.collect {
        case Page.Child.OfLine(siblingLine) if siblingLine != line => Siblings.UnderPage.OfLine(siblingLine)
        case Page.Child.OfTable(table)                             => Siblings.UnderPage.OfTable(table)
      }
    }

  def siblingsOf(keyValueSet: KeyValueSet): ExceptionOr[ArraySeq[KeyValueSet]] =
    parentOf(keyValueSet).map(p =>
      p.children.collect {
        case Page.Child.OfKeyValueSet(kvSet) => kvSet
      },
    )

  def siblingsOf(page: Page): ExceptionOr[ArraySeq[Page]] =
    if (analysisResult.pages.contains(page)) {
      Right(analysisResult.pages.filter(p => p != page))
    } else {
      Left(NotFoundInResults(page))
    }

  def findCell(
    table: Table,
    columnIndex: Int,
    rowIndex: Int,
  ): ExceptionOr[Table.Cell] =
    for {
      cellLookup <-
        tableCellLookup
          .get(table)
          .toRight(NotFoundInResults(table))

      cell <-
        cellLookup
          .get((columnIndex, rowIndex))
          .toRight(GenericException(s"Cell $columnIndex, $rowIndex not found"))
    } yield cell

  def kvSetFor(key: KeyValueSet.Key): ExceptionOr[KeyValueSet] =
    kvSetsForKeys.get(key).toRight(NotFoundInResults(key))

  def valueFor(key: KeyValueSet.Key): ExceptionOr[KeyValueSet.Value] = kvSetFor(key).map(_.value)

  def kvSetFor(value: KeyValueSet.Value): ExceptionOr[KeyValueSet] =
    kvSetsForValues.get(value).toRight(NotFoundInResults(value))

  def keyFor(value: KeyValueSet.Value): ExceptionOr[KeyValueSet.Key] = kvSetFor(value).map(_.key)

  private def untypedParentOf(block: Block): ExceptionOr[Option[Block]] =
    block match {
      case block: AtomicBlock =>
        parentOf(block).map {
          case ForAtomicBlock.OfLine(line) => Some(line)
          case ForAtomicBlock.OfCell(cell) => Some(cell)
        }
      case line: Line                                          => parentOf(line).map(Some.apply)
      case table: Table                                        => parentOf(table).map(Some.apply)
      case cell: Table.Cell                                    => parentOf(cell).map(Some.apply)
      case _: Page | _: KeyValueSet.Key | _: KeyValueSet.Value => Right(None)
    }

  def recursivelySearchChildrenOf[B2 <: Block](
    blocks: Seq[Block],
  )(
    collect: PartialFunction[Block, B2],
  ): ExceptionOr[LazyList[B2]] = {
    def isParent(maybeParent: Block, maybeChild: Block): ExceptionOr[Boolean] =
      untypedParentOf(maybeChild).flatMap {
        case None => Right(false)
        case Some(parent) =>
          if (parent == maybeParent) {
            Right(true)
          } else {
            isParent(maybeParent, parent)
          }
      }

    @tailrec
    def unsafeRemoveChildBlocks(blocks: Seq[Block], blocksWithoutChildren: ListSet[Block]): ListSet[Block] =
      blocks match {
        case Seq() => blocksWithoutChildren
        case candidate +: remainingBlocks =>
          if (blocks.exists(b => isParent(b, candidate).getOrThrow)) {
            unsafeRemoveChildBlocks(remainingBlocks, blocksWithoutChildren)
          } else {
            val blocksToRemove = blocks.filter(b => isParent(candidate, b).getOrThrow).toSet

            unsafeRemoveChildBlocks(remainingBlocks, blocksWithoutChildren.diff(blocksToRemove).incl(candidate))
          }
      }

    for {
      independentRootBlocks <-
        ExceptionOr.catchIn(unsafeRemoveChildBlocks(blocks, blocksWithoutChildren = ListSet.empty))
    } yield independentRootBlocks.to(LazyList).flatMap(b => recursivelySearchChildrenOf[B2](b)(collect))
  }

  def recursivelySearchChildrenOf[B2 <: Block](block: Block)(collect: PartialFunction[Block, B2]): LazyList[B2] =
    LazyList.from {
      BlockIterator.recursivelyIterateBlockAndChildren(block, includeKeyValueSets = true).collect(collect)
    }

  def searchAllResults[B2 <: Block](collect: PartialFunction[Block, B2]): LazyList[B2] =
    analysisResult.pages.to(LazyList).flatMap(p => recursivelySearchChildrenOf(p)(collect))

  val syntax: ResultNavigator.Syntax = new ResultNavigator.Syntax(this)

}

object ResultNavigator {

  final case class NoParentFor(block: Block)       extends ProductException
  final case class NotFoundInResults(block: Block) extends ProductException

  object Parent {

    sealed trait ForAtomicBlock

    object ForAtomicBlock {
      final case class OfLine(line: Line)       extends Parent.ForAtomicBlock
      final case class OfCell(cell: Table.Cell) extends Parent.ForAtomicBlock
    }

  }

  object Siblings {
    sealed trait UnderPage

    object UnderPage {
      final case class OfLine(line: Line)    extends Siblings.UnderPage
      final case class OfTable(table: Table) extends Siblings.UnderPage
    }
  }

  /**
    * Returns children of the given block, excluding any child key-value sets.
    */
  def enclosedChildrenOf(block: Block): ArraySeq[Block] =
    block match {
      case _: AtomicBlock => ArraySeq.empty
      case line: Line     => line.children
      case page: Page =>
        page.children.collect {
          case Child.OfLine(line)   => line
          case Child.OfTable(table) => table
        }
      case table: Table             => table.children
      case cell: Table.Cell         => cell.children
      case key: KeyValueSet.Key     => key.children
      case value: KeyValueSet.Value => value.children
    }

  def apply(analysisResult: AnalysisResult): ResultNavigator = {
    val atomicBlockParents: mutable.Map[AtomicBlock, Parent.ForAtomicBlock] = mutable.Map()
    val cellParents: mutable.Map[Table.Cell, Table] = mutable.Map()
    val tableParents: mutable.Map[Table, Page] = mutable.Map()
    val lineParents: mutable.Map[Line, Page] = mutable.Map()
    val kvSetParents: mutable.Map[KeyValueSet, Page] = mutable.Map()

    val tableCellLookup: mutable.Map[Table, Map[(Int, Int), Table.Cell]] = mutable.Map()
    val kvForKeyLookup: mutable.Map[KeyValueSet.Key, KeyValueSet] = mutable.Map()
    val kvForValueLookup: mutable.Map[KeyValueSet.Value, KeyValueSet] = mutable.Map()

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
        case Child.OfKeyValueSet(keyValueSet) => {
          kvSetParents.put(keyValueSet, page)
          kvForKeyLookup.put(keyValueSet.key, keyValueSet)
          kvForValueLookup.put(keyValueSet.value, keyValueSet)
        }
      }
    }

    new ResultNavigator(
      analysisResult,
      atomicBlockParents,
      cellParents,
      tableParents,
      lineParents,
      kvSetParents,
      tableCellLookup,
      kvForKeyLookup,
      kvForValueLookup,
    )
  }

  final class Syntax private[ResultNavigator] (resultNavigator: ResultNavigator) {

    implicit class AnalysisResultOps(analysisResult: AnalysisResult) {
      def recursivelySearch[B2 <: Block](collect: PartialFunction[Block, B2]): ExceptionOr[LazyList[B2]] =
        Either.cond(
          analysisResult == resultNavigator.analysisResult,
          resultNavigator.searchAllResults[B2](collect),
          GenericException("Not the parsed analysis results"),
        )
    }

    implicit class BlocksOps[B <: Block : ClassTag](blocks: Seq[B]) {
      def searchRecursively[B2 <: Block](collect: PartialFunction[Block, B2]): ExceptionOr[LazyList[B2]] =
        resultNavigator.recursivelySearchChildrenOf(blocks)(collect)
    }

    implicit class KeyOps(key: KeyValueSet.Key) extends BlockOps[KeyValueSet.Key](key, resultNavigator) {
      def parent: ExceptionOr[Page] = resultNavigator.parentOf(key)
      def kvSet: ExceptionOr[KeyValueSet] = resultNavigator.kvSetFor(key)
      def value: ExceptionOr[KeyValueSet.Value] = resultNavigator.valueFor(key)
    }

    implicit class ValueOps(value: KeyValueSet.Value) extends BlockOps[KeyValueSet.Value](value, resultNavigator) {
      def parent: ExceptionOr[Page] = resultNavigator.parentOf(value)
      def kvSet: ExceptionOr[KeyValueSet] = resultNavigator.kvSetFor(value)
      def key: ExceptionOr[KeyValueSet.Key] = resultNavigator.keyFor(value)
    }

    implicit class KeyValueSetOps(kvSet: KeyValueSet) {
      def parent: ExceptionOr[Page] = resultNavigator.parentOf(kvSet)
      def siblings: ExceptionOr[ArraySeq[KeyValueSet]] = resultNavigator.siblingsOf(kvSet)
    }

    implicit class AtomicBlockOps(atomicBlock: AtomicBlock)
        extends BlockOps[AtomicBlock](atomicBlock, resultNavigator) {
      def parent: ExceptionOr[Parent.ForAtomicBlock] =
        resultNavigator.parentOf(atomicBlock)

      def siblings: ExceptionOr[ArraySeq[AtomicBlock]] =
        resultNavigator.siblingsOf(atomicBlock)
    }

    implicit class CellOps(cell: Table.Cell) extends BlockOps[Table.Cell](cell, resultNavigator) {
      def parent: ExceptionOr[Table] =
        resultNavigator.parentOf(cell)

      def siblings: ExceptionOr[ArraySeq[Table.Cell]] =
        resultNavigator.siblingsOf(cell)
    }

    implicit class TableOps(table: Table) extends BlockOps[Table](table, resultNavigator) {
      def parent: ExceptionOr[Page] =
        resultNavigator.parentOf(table)

      def siblings: ExceptionOr[ArraySeq[Siblings.UnderPage]] =
        resultNavigator.siblingsOf(table)

      def findCell(columnIndex: Int, rowIndex: Int): ExceptionOr[Table.Cell] =
        resultNavigator.findCell(table, columnIndex, rowIndex)
    }

    implicit class LineOps(line: Line) extends BlockOps[Line](line, resultNavigator) {
      def parent: ExceptionOr[Page] =
        resultNavigator.parentOf(line)

      def siblings: ExceptionOr[ArraySeq[Siblings.UnderPage]] =
        resultNavigator.siblingsOf(line)
    }

    implicit class PageOps(page: Page) extends BlockOps[Page](page, resultNavigator) {
      def siblings: ExceptionOr[ArraySeq[Page]] = resultNavigator.siblingsOf(page)
    }

  }

  object Syntax {

    abstract class BlockOps[B <: Block : ClassTag] private[Syntax] (block: B, resultNavigator: ResultNavigator) {
      def searchRecursively[B2 <: Block](collect: PartialFunction[Block, B2]): LazyList[B2] =
        resultNavigator.recursivelySearchChildrenOf[B2](block)(collect)

      def searchRecursivelyUsingPredicate[B2 <: Block : ClassTag](predicate: B2 => Boolean): LazyList[B2] =
        resultNavigator.recursivelySearchChildrenOf[B2](block) {
          case b2: B2 if predicate(b2) => b2
        }
    }

  }

}
