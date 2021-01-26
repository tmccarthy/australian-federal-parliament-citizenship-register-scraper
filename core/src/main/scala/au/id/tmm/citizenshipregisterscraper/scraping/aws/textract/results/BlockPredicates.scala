package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._

import scala.collection.immutable.ArraySeq

object BlockPredicates {

  def hasWordsLike(searchText: String)(line: Line): Boolean = hasWordsLike(searchText, line.children)
  def hasWordsLike(searchText: String)(cell: Table.Cell): Boolean = hasWordsLike(searchText, cell.children)
  def hasWordsLike(searchText: String)(key: KeyValueSet.Key): Boolean = hasWordsLike(searchText, key.children)
  def hasWordsLike(searchText: String)(value: KeyValueSet.Value): Boolean = hasWordsLike(searchText, value.children)

  def hasWordsLike(searchText: String, atomicBlocks: ArraySeq[AtomicBlock]): Boolean = {
    val blockWords = atomicBlocks.flatMap {
      case _: SelectionElement => ArraySeq.empty
      case w: Word             => ArraySeq(w.text.toLowerCase)
    }

    ArraySeq.unsafeWrapArray(searchText.split("""\s+""").map(_.toLowerCase)).containsSlice(blockWords)
  }

}
