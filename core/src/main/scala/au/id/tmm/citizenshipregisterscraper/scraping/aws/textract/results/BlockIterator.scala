package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{AtomicBlock, Block, KeyValueSet, Line, Page, Table}

object BlockIterator {

  def recursivelyIterateBlockAndChildren(block: Block): Iterator[Block] =
    Iterator(block) ++ recursivelyIterateChildrenOf(block)

  def recursivelyIterateChildrenOf(block: Block): Iterator[Block] =
    block match {
      case block: AtomicBlock => Iterator.empty[Block]
      case line: Line => line.children.flatMap(c => recursivelyIterateBlockAndChildren(c)).iterator
      case page: Page => page.children
        .flatMap {
          case Page.Child.OfLine(line) => recursivelyIterateBlockAndChildren(line)
          case Page.Child.OfTable(table) => recursivelyIterateBlockAndChildren(table)
          case Page.Child.OfKeyValueSet(keyValueSet) =>
            recursivelyIterateBlockAndChildren(keyValueSet.key) ++ recursivelyIterateBlockAndChildren(keyValueSet.value)
        }
        .iterator
      case table: Table => table.children.flatMap(c => recursivelyIterateBlockAndChildren(c)).iterator
      case cell: Table.Cell => cell.children.flatMap(c => recursivelyIterateBlockAndChildren(c)).iterator
      case key: KeyValueSet.Key => key.children.flatMap(c => recursivelyIterateBlockAndChildren(c)).iterator
      case value: KeyValueSet.Value => value.children.flatMap(c => recursivelyIterateBlockAndChildren(c)).iterator
    }

}
