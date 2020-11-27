package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.parsing

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{BlockId, KeyValueSet, PageNumber, Word}
import au.id.tmm.collections.syntax.toIterableOps
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import cats.syntax.apply._
import cats.syntax.traverse.toTraverseOps
import cats.syntax.traverseFilter.toTraverseFilterOps
import software.amazon.awssdk.services.textract.{model => sdk}

import scala.collection.immutable.ArraySeq

object KeyValueSets {

  import Common._
  import Relationships._

  def extractKeyValueSets(
    wordLookup: Map[BlockId, Word],
    allBlocks: ArraySeq[sdk.Block],
  ): ExceptionOr[ArraySeq[KeyValueSet]] =
    for {
      kvSetBlocks <- Right(allBlocks.filter(_.blockType == sdk.BlockType.KEY_VALUE_SET))
      kvSetBlocksById <-
        kvSetBlocks
          .traverse(b => BlockId.fromString(b.id).map(_ -> b))
          .map(_.toMap)

      keyValueSets <-
        kvSetBlocks
          .traverseFilter { block =>
            for {
              isKey <- isKeyBlock(block)
              maybeKeyValueSet <-
                if (isKey) {
                  parseKeyValueSet(wordLookup, kvSetBlocksById, block).map(Some.apply)
                } else {
                  Right(None)
                }
            } yield maybeKeyValueSet
          }

    } yield keyValueSets

  private def parseKeyValueSet(
    wordLookup: Map[BlockId, Word],
    kvSetBlocksLookup: Map[BlockId, sdk.Block],
    keyBlock: sdk.Block,
  ): ExceptionOr[KeyValueSet] =
    for {
      key            <- parseKey(wordLookup, keyBlock)
      valueSdkBlocks <- lookupOrFail(kvSetBlocksLookup, keyBlock, sdk.RelationshipType.VALUE)
      valueSdkBlock  <- valueSdkBlocks.onlyElementOrException
      value          <- parseValue(wordLookup, valueSdkBlock)
    } yield KeyValueSet(key, value)

  private def parseKey(
    wordLookup: Map[BlockId, Word],
    keyBlock: sdk.Block,
  ): ExceptionOr[KeyValueSet.Key] =
    for {
      id         <- BlockId.fromString(keyBlock.id)
      pageNumber <- PageNumber(keyBlock.page)
      geometry   <- parseGeometry(keyBlock.geometry)
      words      <- lookupOrFail(wordLookup, keyBlock, sdk.RelationshipType.CHILD)
    } yield KeyValueSet.Key(
      id,
      pageNumber,
      geometry,
      words,
    )

  private def parseValue(
    wordLookup: Map[BlockId, Word],
    valueSdkBlock: sdk.Block,
  ): ExceptionOr[KeyValueSet.Value] =
    for {
      _          <- requireValueBlock(valueSdkBlock)
      id         <- BlockId.fromString(valueSdkBlock.id)
      pageNumber <- PageNumber(valueSdkBlock.page)
      geometry   <- parseGeometry(valueSdkBlock.geometry)
      words      <- lookupOrFail(wordLookup, valueSdkBlock, sdk.RelationshipType.CHILD)
    } yield KeyValueSet.Value(
      id,
      pageNumber,
      geometry,
      words,
    )

  private def isKeyBlock(sdkBlock: sdk.Block): ExceptionOr[Boolean] =
    (requireNonNull(sdkBlock.blockType), hasEntityType(sdkBlock, sdk.EntityType.KEY)).mapN {
      (blockType, hasEntityType) =>
        hasEntityType && blockType == sdk.BlockType.KEY_VALUE_SET
    }

  private def requireValueBlock(sdkBlock: sdk.Block): ExceptionOr[Unit] =
    isValueBlock(sdkBlock).map { valueBlockCheck =>
      Either.cond(valueBlockCheck, (), GenericException("Expected value block"))
    }

  private def isValueBlock(sdkBlock: sdk.Block): ExceptionOr[Boolean] =
    (requireNonNull(sdkBlock.blockType), hasEntityType(sdkBlock, sdk.EntityType.VALUE)).mapN {
      (blockType, hasEntityType) =>
        hasEntityType && blockType == sdk.BlockType.KEY_VALUE_SET
    }

  private def hasEntityType(sdkBlock: sdk.Block, entityType: sdk.EntityType): ExceptionOr[Boolean] =
    Option(sdkBlock.entityTypes).map(_.size) match {
      case None | Some(0) => Right(false)
      case Some(1)        => Right(sdkBlock.entityTypes.get(0) == entityType)
      case Some(_)        => Left(GenericException("Multiple entity types"))
    }

}
