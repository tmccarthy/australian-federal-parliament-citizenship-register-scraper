package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._

import scala.collection.immutable.ArraySeq

object BlockPredicates {

  def lineHasWordsLike(searchText: String)(line: Line): Boolean = hasWordsLike(searchText, line.children)
  def cellHasWordsLike(searchText: String)(cell: Table.Cell): Boolean = hasWordsLike(searchText, cell.children)
  def keyHasWordsLike(searchText: String)(key: KeyValueSet.Key): Boolean = hasWordsLike(searchText, key.children)
  def valueHasWordsLike(searchText: String)(value: KeyValueSet.Value): Boolean =
    hasWordsLike(searchText, value.children)

  def hasWordsLike(searchText: String, atomicBlocks: ArraySeq[AtomicBlock]): Boolean = {
    val blockWords = atomicBlocks.flatMap {
      case _: SelectionElement => ArraySeq.empty
      case w: Word             => reduceToSimpleTextArray(w.text)
    }

    blockWords.containsSlice(reduceToSimpleTextArray(searchText))
  }

  private def reduceToSimpleTextArray(string: String): ArraySeq[String] =
    ArraySeq.unsafeWrapArray(string.toLowerCase.replaceAll("""[^\w\s]""", "").split("""\s+"""))

}
