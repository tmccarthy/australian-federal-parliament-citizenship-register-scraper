package au.id.tmm.citizenshipregisterscraper.scraping

import java.util.NoSuchElementException

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{AtomicBlock, Block, KeyValueSet, Line, Page, SelectionElement, Table, Word}
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

// TODO rename
// TODO move to better location
object ScrapingUtilities {

  def getOrFail[A](seq: Seq[A], index: Int): ExceptionOr[A] =
    seq.lift(index).toRight(new IndexOutOfBoundsException(index))

  def iterateChildren(rootBlock: Block): Iterator[Block] = {
    Iterator(rootBlock) ++ (rootBlock match {
      case block: AtomicBlock => Iterator.empty[Block]
      case line: Line => line.children.flatMap(c => iterateChildren(c))
      case page: Page => page.children.flatMap {
        case Page.Child.OfLine(line) => iterateChildren(line)
        case Page.Child.OfTable(table) => iterateChildren(table)
        case Page.Child.OfKeyValueSet(keyValueSet) =>
          iterateChildren(keyValueSet.key) ++ iterateChildren(keyValueSet.value)
      }
      case table: Table => table.children.flatMap(c => iterateChildren(c))
      case cell: Table.Cell => cell.children.flatMap(c => iterateChildren(c))
      case key: KeyValueSet.Key => key.children.flatMap(c => iterateChildren(c))
      case value: KeyValueSet.Value => value.children.flatMap(c => iterateChildren(c))
    })
  }

  def onlyBlockMatching[B <: Block : ClassTag](
    rootBlock: Block,
    predicate: B => Boolean,
  ): ExceptionOr[B] = {
    val lazyList = blocksMatching[B](rootBlock, predicate)

    lazyList.headOption match {
      case None => Left(new NoSuchElementException())
      case Some(firstMatchingElement) =>
        if (lazyList.tail.isEmpty) {
          Right(firstMatchingElement)
        } else {
          Left(new IllegalStateException("Too many matches"))
        }
    }
  }

  def firstBlockMatching[B <: Block : ClassTag](
    rootBlock: Block,
    predicate: B => Boolean,
  ): ExceptionOr[B] = {
    val lazyList = blocksMatching[B](rootBlock, predicate)

    lazyList.headOption match {
      case None => Left(new NoSuchElementException())
      case Some(firstMatchingElement) => Right(firstMatchingElement)
    }
  }

  def hasBlocksMatching[B <: Block : ClassTag](
    rootBlock: Block,
    predicate: B => Boolean,
  ): Boolean = blocksMatching[B](rootBlock, predicate).nonEmpty

  def blocksMatching[B <: Block : ClassTag](
    rootBlock: Block,
    predicate: B => Boolean,
  ): LazyList[B] = {
    LazyList.from(iterateChildren(rootBlock))
      .collect {
        case b: B if predicate(b) => b
      }
  }

  def hasWordsLike(searchText: String, rootBlock: Block): Boolean = {
    hasBlocksMatching[Block](
      rootBlock,
      {
        case block: AtomicBlock => hasWordsLike(searchText, ArraySeq(block))
        case line: Line => hasWordsLike(searchText, line.children)
        case cell: Table.Cell => hasWordsLike(searchText, cell.children)
        case _: KeyValueSet.Key | _: KeyValueSet.Value | _: Table | _: Page => false
      }
    )
  }

  def hasWordsLike(searchText: String, atomicBlocks: ArraySeq[AtomicBlock]): Boolean = {
    val blockWords = atomicBlocks.flatMap {
      case _: SelectionElement => ArraySeq.empty
      case w: Word => ArraySeq(w.text.toLowerCase)
    }

    ArraySeq.unsafeWrapArray(searchText.split("""\s+""").map(_.toLowerCase)).containsSlice(blockWords)
  }

}
