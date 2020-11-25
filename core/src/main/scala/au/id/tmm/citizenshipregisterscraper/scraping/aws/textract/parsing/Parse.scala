package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.parsing

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.utilities.errors.ExceptionOr
import au.id.tmm.utilities.errors.syntax._
import cats.syntax.traverse.toTraverseOps
import cats.syntax.traverseFilter.toTraverseFilterOps
import software.amazon.awssdk.services.textract.{model => sdk}

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

// TODO rename
object Parse {

  import Common._

  def toModel(apiResponses: ArraySeq[sdk.GetDocumentAnalysisResponse]): ExceptionOr[AnalysisResult] =
    for {
      allBlocks <-
        apiResponses
          .flatTraverse[ExceptionOr, sdk.Block] { r =>
            requireNonNull(r.blocks).map(_.asScala.to(ArraySeq))
          }

      pages <- Relationships.enrichAnyBlockNotFoundFailures(allBlocks, extractPages(allBlocks))
    } yield AnalysisResult(pages)

  private def extractPages(allBlocks: ArraySeq[sdk.Block]): ExceptionOr[ArraySeq[Page]] =
    for {
      wordsById <- makeLookup[Word](allBlocks, sdk.BlockType.WORD, Words.parseWord)

      selectionElementsById <-
        makeLookup[SelectionElement](allBlocks, sdk.BlockType.SELECTION_ELEMENT, SelectionElements.parseSelectionElement)

      linesById <- makeLookup[Line](allBlocks, sdk.BlockType.LINE, Lines.parseLine(wordsById, selectionElementsById, _))

      cellById   <- makeLookup[Table.Cell](allBlocks, sdk.BlockType.CELL, Tables.parseCell(wordsById, _))
      tablesById <- makeLookup[Table](allBlocks, sdk.BlockType.TABLE, Tables.parseTable(cellById, _))

      keyValueSetsById: Map[BlockId, KeyValueSet] = Map.empty // TODO parse these

      pages <- extract[Page](allBlocks, sdk.BlockType.PAGE, Pages.parsePage(linesById, tablesById, keyValueSetsById, _))
    } yield pages

  private def extract[B <: HasBlockId : ClassTag](
    blocks: ArraySeq[sdk.Block],
    blockType: sdk.BlockType,
    make: sdk.Block => ExceptionOr[B],
  ): ExceptionOr[ArraySeq[B]] =
    blocks
      .traverseFilter {
        case sdkBlock if sdkBlock.blockType() == blockType =>
          make(sdkBlock)
            .map(Some(_))
            .wrapExceptionWithMessage(s"Failure while parsing a $blockType")
        case _ => Right(None)
      }

  private def makeLookup[B <: HasBlockId : ClassTag](
    blocks: ArraySeq[sdk.Block],
    blockType: sdk.BlockType,
    make: sdk.Block => ExceptionOr[B],
  ): ExceptionOr[Map[BlockId, B]] =
    extract[B](blocks, blockType, make)
      .map(lookupById[B])

  private def lookupById[B <: HasBlockId](bs: ArraySeq[B]): Map[BlockId, B] =
    bs.map(b => b.id -> b).toMap

}
