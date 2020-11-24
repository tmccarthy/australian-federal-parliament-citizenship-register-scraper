package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Geometry.BoundingBox
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import cats.syntax.traverse.toTraverseOps
import software.amazon.awssdk.services.textract.{model => sdk}

import scala.collection.immutable.{ArraySeq, SortedSet}
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

// TODO rename
object Parse {

  def toModel(apiResponse: sdk.GetDocumentAnalysisResponse): ExceptionOr[AnalysisResult] = {

    for {
      allBlocks <- requireNonNull(apiResponse.blocks).map(_.asScala.to(ArraySeq))
      wordsById <- makeLookup[Word](allBlocks, sdk.BlockType.WORD, parseWord)

      selectionElementsById <- makeLookup[SelectionElement](allBlocks, sdk.BlockType.SELECTION_ELEMENT, parseSelectionElement)

      linesById <- makeLookup[Line](allBlocks, sdk.BlockType.LINE, parseLine(wordsById, _))

      cellById <- makeLookup[Table.Cell](allBlocks, sdk.BlockType.CELL, parseCell(wordsById, _))
      tablesById <- makeLookup[Table](allBlocks, sdk.BlockType.TABLE, parseTable(cellById, _))

      keyValueSetsById <- makeLookup[KeyValueSet](allBlocks, sdk.BlockType.KEY_VALUE_SET, parseKeyValueSet(wordsById, _))

      pages <- extract[Page](allBlocks, sdk.BlockType.PAGE, parsePage(linesById, tablesById, keyValueSetsById, _))
    } yield AnalysisResult(SortedSet.from(pages), AnalysisResult.DocumentMetadata(apiResponse.documentMetadata.pages))
  }

  private def extract[B <: HasBlockId : ClassTag](
    blocks: ArraySeq[sdk.Block],
    blockType: sdk.BlockType,
    make: sdk.Block => ExceptionOr[B],
  ): ExceptionOr[ArraySeq[B]] =
    blocks
      .flatTraverse {
        case sdkBlock if sdkBlock.blockType() == blockType =>
          make(sdkBlock).map(ArraySeq(_))
        case _ => Right(ArraySeq.empty)
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

  private def parseCell(
    wordLookup: Map[BlockId, Word],
    block: sdk.Block,
  ): ExceptionOr[Table.Cell] = ???

  private def parseTable(
    cellLookup: Map[BlockId, Table.Cell],
    block: sdk.Block,
  ): ExceptionOr[Table] = ???

  private def parseKeyValueSet(
    wordLookup: Map[BlockId, Word],
    block: sdk.Block,
  ): ExceptionOr[KeyValueSet] = ???

  private def parsePage(
    lineLookup: Map[BlockId, Line],
    tableLookup: Map[BlockId, Table],
    keyValueSetLookup: Map[BlockId, KeyValueSet],
    block: sdk.Block,
  ): ExceptionOr[Page] = ???

  private def parseLine(
    wordLookup: Map[BlockId, Word],
    block: sdk.Block,
  ): ExceptionOr[Line] = {
    for {
      _ <- requireBlockType(block, sdk.BlockType.WORD)
      id <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry <- parseGeometry(block.geometry)
      text <- requireNonNull(block.text)
      words <- lookupOrFail(wordLookup, block.relationships, sdk.RelationshipType.CHILD)
    } yield Line(
      id,
      pageNumber,
      geometry,
      text,
      words,
    )
  }

  private def parseSelectionElement(block: sdk.Block): ExceptionOr[SelectionElement] =
    for {
      _ <- requireBlockType(block, sdk.BlockType.SELECTION_ELEMENT)
      id <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry <- parseGeometry(block.geometry)
      status <- block.selectionStatus match {
        case sdk.SelectionStatus.SELECTED => Right(SelectionElement.Status.Selected)
        case sdk.SelectionStatus.NOT_SELECTED => Right(SelectionElement.Status.NotSelected)
        case sdk.SelectionStatus.UNKNOWN_TO_SDK_VERSION => Left(GenericException("Unknown selection status"))
      }
    } yield SelectionElement(id, pageNumber, geometry, status)

  private def parseWord(block: sdk.Block): ExceptionOr[Word] =
    for {
      _ <- requireBlockType(block, sdk.BlockType.WORD)
      id <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry <- parseGeometry(block.geometry)
      text <- requireNonNull(block.text)
      confidence <- Confidence(block.confidence)
      textType <- block.textType match {
        case sdk.TextType.HANDWRITING => Right(Word.TextType.Handwriting)
        case sdk.TextType.PRINTED => Right(Word.TextType.Printed)
        case sdk.TextType.UNKNOWN_TO_SDK_VERSION => Left(GenericException("Unknown text type"))
      }
    } yield Word(id, pageNumber, geometry, text, confidence, textType)

  private def parseGeometry(sdkGeometry: sdk.Geometry): ExceptionOr[Geometry] =
    for {
      sdkBoundingBox <- requireNonNull(sdkGeometry.boundingBox)
      boundingBox <- BoundingBox(
        sdkBoundingBox.left,
        sdkBoundingBox.top,
        sdkBoundingBox.height,
        sdkBoundingBox.width,
      )
      sdkPoints <- requireNonNull(sdkGeometry.polygon()).map(_.asScala.to(ArraySeq))
      points <- sdkPoints.traverse(p => Geometry.Polygon.Point(p.x, p.y))
    } yield Geometry(boundingBox, Geometry.Polygon(points))

  private def requireNonNull[A](a: A): ExceptionOr[A] =
    if (a == null) {
      Left(new NullPointerException)
    } else {
      Right(a)
    }

  private def requireBlockType(block: sdk.Block, expectedType: sdk.BlockType): ExceptionOr[Unit] =
    if (block.blockType() == expectedType) {
      Right(())
    } else {
      Left(GenericException(s"Expected $expectedType, but was ${block.blockType}"))
    }

  private def lookupOrFail[B <: HasBlockId](
    lookup: Map[BlockId, B],
    relationships: java.util.List[sdk.Relationship],
    relationshipType: sdk.RelationshipType,
  ): ExceptionOr[ArraySeq[B]] =
    for {
      idsAsStrings <- relationships.asScala
        .to(ArraySeq)
        .flatTraverse[ExceptionOr, String] {
          case r if r.`type` == relationshipType => Right(r.ids.asScala.to(ArraySeq))
          case unexpectedRelationship => Left(GenericException(s"Unexpected relationship type ${unexpectedRelationship.`type`}"))
        }

      ids <- idsAsStrings.traverse(BlockId.fromString)

      blocks <- ids.traverse { blockId =>
        lookup.get(blockId).toRight(GenericException(s"Couldn't find block with id $blockId in lookup"))
      }
    } yield blocks

}
