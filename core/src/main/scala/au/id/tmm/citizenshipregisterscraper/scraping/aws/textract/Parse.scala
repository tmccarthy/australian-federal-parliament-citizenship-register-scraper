package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Geometry.BoundingBox
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.utilities.errors.syntax._
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException, ProductException}
import cats.syntax.traverse.toTraverseOps
import com.github.ghik.silencer.silent
import software.amazon.awssdk.services.textract.{model => sdk}

import scala.collection.MapView
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import cats.syntax.monadError._

// TODO rename
object Parse {

  def toModel(apiResponse: sdk.GetDocumentAnalysisResponse): ExceptionOr[AnalysisResult] =
    for {
      allBlocks <- requireNonNull(apiResponse.blocks).map(_.asScala.to(ArraySeq))

      pages <- extractPages(allBlocks).adaptError(clarifyBadBlock(allBlocks))
    } yield AnalysisResult(pages)

  private def clarifyBadBlock(allBlocks: ArraySeq[sdk.Block]): PartialFunction[Exception, Exception] = {
    case PartialBlockNotFoundException(badBlockId) => {
      val foundBlockType = allBlocks.collectFirst {
        case b if BlockId.fromString(b.id).contains(badBlockId) => b.blockType
      }

      BlockNotFoundException(badBlockId, foundBlockType)
    }
    case e @ GenericException(_, Some(cause: Exception)) =>
      e.copy(cause = clarifyBadBlock(allBlocks).lift(cause).orElse(e.cause))
  }

  private def extractPages(allBlocks: ArraySeq[sdk.Block]): ExceptionOr[ArraySeq[Page]] =
    for {
      wordsById <- makeLookup[Word](allBlocks, sdk.BlockType.WORD, parseWord)

      selectionElementsById <-
        makeLookup[SelectionElement](allBlocks, sdk.BlockType.SELECTION_ELEMENT, parseSelectionElement)

      linesById <- makeLookup[Line](allBlocks, sdk.BlockType.LINE, parseLine(wordsById, selectionElementsById, _))

      cellById   <- makeLookup[Table.Cell](allBlocks, sdk.BlockType.CELL, parseCell(wordsById, _))
      tablesById <- makeLookup[Table](allBlocks, sdk.BlockType.TABLE, parseTable(cellById, _))

      keyValueSetsById: Map[BlockId, KeyValueSet] = Map.empty // TODO parse these

      pages <- extract[Page](allBlocks, sdk.BlockType.PAGE, parsePage(linesById, tablesById, keyValueSetsById, _))
    } yield pages

  private def extract[B <: HasBlockId : ClassTag](
    blocks: ArraySeq[sdk.Block],
    blockType: sdk.BlockType,
    make: sdk.Block => ExceptionOr[B],
  ): ExceptionOr[ArraySeq[B]] =
    blocks
      .flatTraverse {
        case sdkBlock if sdkBlock.blockType() == blockType =>
          make(sdkBlock)
            .map(ArraySeq(_))
            .wrapExceptionWithMessage(s"Failure while parsing a $blockType")
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

  private def parseTable(
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

  private def parsePage(
    lineLookup: Map[BlockId, Line],
    tableLookup: Map[BlockId, Table],
    keyValueSetLookup: Map[BlockId, KeyValueSet],
    block: sdk.Block,
  ): ExceptionOr[Page] =
    for {
      _          <- requireBlockType(block, sdk.BlockType.PAGE)
      id         <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry   <- parseGeometry(block.geometry)

      childLookup: Map[BlockId, Page.Child] = (
          (lineLookup.view.mapValues(Page.Child.OfLine): MapView[BlockId, Page.Child]) ++
            (tableLookup.view.mapValues(Page.Child.OfTable): MapView[BlockId, Page.Child])
      ).toMap

      // TODO support key value sets
      children <- lookupOrIgnore(childLookup, block.relationships, sdk.RelationshipType.CHILD)
    } yield Page(
      id,
      pageNumber,
      geometry,
      children,
    )

  private def parseLine(
    wordLookup: Map[BlockId, Word],
    selectionElementLookup: Map[BlockId, SelectionElement],
    block: sdk.Block,
  ): ExceptionOr[Line] =
    for {
      _          <- requireBlockType(block, sdk.BlockType.LINE)
      id         <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry   <- parseGeometry(block.geometry)
      text       <- requireNonNull(block.text)
      childLookup: Map[BlockId, Line.Child] = (
        (wordLookup.view.mapValues(Line.Child.OfWord): MapView[BlockId, Line.Child]) ++
          (selectionElementLookup.view.mapValues(Line.Child.OfSelectionElement): MapView[BlockId, Line.Child])
      ).toMap
      children   <- lookupOrFail(childLookup, block.relationships, sdk.RelationshipType.CHILD)
    } yield Line(
      id,
      pageNumber,
      geometry,
      text,
      children,
    )

  private def parseSelectionElement(block: sdk.Block): ExceptionOr[SelectionElement] =
    for {
      _          <- requireBlockType(block, sdk.BlockType.SELECTION_ELEMENT)
      id         <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry   <- parseGeometry(block.geometry)
      status <- block.selectionStatus match {
        case sdk.SelectionStatus.SELECTED               => Right(SelectionElement.Status.Selected)
        case sdk.SelectionStatus.NOT_SELECTED           => Right(SelectionElement.Status.NotSelected)
        case sdk.SelectionStatus.UNKNOWN_TO_SDK_VERSION => Left(GenericException("Unknown selection status"))
      }
    } yield SelectionElement(id, pageNumber, geometry, status)

  private def parseWord(block: sdk.Block): ExceptionOr[Word] =
    for {
      _          <- requireBlockType(block, sdk.BlockType.WORD)
      id         <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry   <- parseGeometry(block.geometry)
      text       <- requireNonNull(block.text)
      confidence <- Confidence(block.confidence)
      textType <- block.textType match {
        case sdk.TextType.HANDWRITING            => Right(Word.TextType.Handwriting)
        case sdk.TextType.PRINTED                => Right(Word.TextType.Printed)
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
      points    <- sdkPoints.traverse(p => Geometry.Polygon.Point(p.x, p.y))
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

  @silent
  private def lookupOrIgnore[B](
    lookup: Map[BlockId, B],
    relationships: java.util.List[sdk.Relationship],
    relationshipType: sdk.RelationshipType,
  ): ExceptionOr[ArraySeq[B]] =
    for {
      ids <- idsFrom(relationships, relationshipType)

      blocks = ids.flatMap(lookup.get)
    } yield blocks

  private def lookupOrFail[B](
    lookup: Map[BlockId, B],
    relationships: java.util.List[sdk.Relationship],
    relationshipType: sdk.RelationshipType,
  ): ExceptionOr[ArraySeq[B]] =
    for {
      ids <- idsFrom(relationships, relationshipType)

      blocks <- ids.traverse { blockId =>
        lookup.get(blockId).toRight(PartialBlockNotFoundException(blockId))
      }
    } yield blocks

  private def idsFrom(
    relationships: java.util.List[sdk.Relationship],
    relationshipType: sdk.RelationshipType,
  ): ExceptionOr[ArraySeq[BlockId]] =
    for {
      idsAsStrings <-
        relationships.asScala
          .to(ArraySeq)
          .flatTraverse[ExceptionOr, String] {
            case r if r.`type` == relationshipType => Right(r.ids.asScala.to(ArraySeq))
            case unexpectedRelationship =>
              Left(GenericException(s"Unexpected relationship type ${unexpectedRelationship.`type`}"))
          }

      ids <- idsAsStrings.traverse(BlockId.fromString)
    } yield ids

  private final case class PartialBlockNotFoundException(badBlockId: BlockId) extends ProductException

  final case class BlockNotFoundException(
    badBlockId: BlockId,
    foundAs: Option[sdk.BlockType],
  ) extends ProductException

}
