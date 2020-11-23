package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{AnalysisResult, BlockId, Confidence, Geometry, PageNumber, SelectionElement, Word}
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import au.id.tmm.utilities.errors.syntax._
import au.id.tmm.utilities.syntax.tuples.->
import software.amazon.awssdk.services.textract.model.{SelectionStatus => ApiSelectionStatus, Block, BlockType, GetDocumentAnalysisResponse, Geometry => ApiGeometry, TextType => ApiTextType}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import cats.syntax.traverse.toTraverseOps

import scala.collection.immutable.ArraySeq

// TODO rename
object Parse {

  def toModel(apiResponse: GetDocumentAnalysisResponse): ExceptionOr[AnalysisResult] = {

    for {
      allBlocks <- requireNonNull(apiResponse.blocks).map(_.asScala.to(ArraySeq))
      wordsById: ArraySeq[BlockId -> Word] <- allBlocks
        .flatTraverse[ExceptionOr, BlockId -> Word] {
          case apiBlock if apiBlock.blockType() == BlockType.WORD =>
            for {
              word <- parseWord(apiBlock)
            } yield ArraySeq(word.id -> word)
          case _ => Right(ArraySeq.empty)
        }

      lookupWordsById: Map[BlockId, Word] = wordsById.toMap

    } yield ???
  }

  private def parseSelectionElement(block: Block): ExceptionOr[SelectionElement] =
    for {
      _ <- requireBlockType(block, BlockType.SELECTION_ELEMENT)
      id <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry <- parseGeometry(block.geometry)
      status <- block.selectionStatus match {
        case ApiSelectionStatus.SELECTED => Right(SelectionElement.Status.Selected)
        case ApiSelectionStatus.NOT_SELECTED => Right(SelectionElement.Status.NotSelected)
        case ApiSelectionStatus.UNKNOWN_TO_SDK_VERSION => Left(GenericException("Unknown selection status"))
      }
    } yield SelectionElement(id, pageNumber, geometry, status)

  private def parseWord(block: Block): ExceptionOr[Word] =
    for {
      _ <- requireBlockType(block, BlockType.WORD)
      id <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry <- parseGeometry(block.geometry)
      text <- requireNonNull(block.text)
      confidence <- Confidence(block.confidence)
      textType <- block.textType match {
        case ApiTextType.HANDWRITING => Right(Word.TextType.Handwriting)
        case ApiTextType.PRINTED => Right(Word.TextType.Printed)
        case ApiTextType.UNKNOWN_TO_SDK_VERSION => Left(GenericException("Unknown text type"))
      }
    } yield Word(id, pageNumber, geometry, text, confidence, textType)

  private def parseGeometry(apiGeometry: ApiGeometry): ExceptionOr[Geometry] = ???

  private def requireNonNull[A](a: A): ExceptionOr[A] =
    if (a == null) {
      Left(new NullPointerException)
    } else {
      Right(a)
    }

  private def requireBlockType(block: Block, expectedType: BlockType): ExceptionOr[Unit] =
    if (block.blockType() == expectedType) {
      Right(())
    } else {
      Left(GenericException(s"Expected $expectedType, but was ${block.blockType}"))
    }

}
