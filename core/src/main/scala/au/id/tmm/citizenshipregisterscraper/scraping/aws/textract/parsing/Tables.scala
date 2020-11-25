package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.parsing

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{BlockId, PageNumber, Table, Word}
import au.id.tmm.utilities.errors.ExceptionOr
import software.amazon.awssdk.services.textract.{model => sdk}

private[parsing] object Tables {

  import Common._
  import Relationships._

  def parseCell(
    wordLookup: Map[BlockId, Word],
    block: sdk.Block,
  ): ExceptionOr[Table.Cell] =
    for {
      _           <- requireBlockType(block, sdk.BlockType.CELL)
      id          <- BlockId.fromString(block.id)
      pageNumber  <- PageNumber(block.page)
      geometry    <- parseGeometry(block.geometry)
      columnIndex <- requireNonNull(block.columnIndex)
      columnSpan  <- requireNonNull(block.columnSpan)
      rowIndex    <- requireNonNull(block.rowIndex)
      rowSpan     <- requireNonNull(block.rowSpan)
      words       <- lookupOrFail(wordLookup, block.relationships, sdk.RelationshipType.CHILD)
    } yield Table.Cell(
      id,
      pageNumber,
      geometry,
      columnIndex,
      columnSpan,
      rowIndex,
      rowSpan,
      words,
    )

  def parseTable(
    cellLookup: Map[BlockId, Table.Cell],
    block: sdk.Block,
  ): ExceptionOr[Table] =
    for {
      _          <- requireBlockType(block, sdk.BlockType.TABLE)
      id         <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry   <- parseGeometry(block.geometry)
      cells      <- lookupOrFail(cellLookup, block.relationships, sdk.RelationshipType.CHILD)
    } yield Table(
      id,
      pageNumber,
      geometry,
      cells,
    )

}
