package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.parsing

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.BlockId
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException, ProductException}
import cats.syntax.traverse.toTraverseOps
import software.amazon.awssdk.services.textract.{model => sdk}

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._

private[parsing] object Relationships {

  def lookupOrIgnore[B](
    lookup: Map[BlockId, B],
    relationships: java.util.List[sdk.Relationship],
    relationshipType: sdk.RelationshipType,
  ): ExceptionOr[ArraySeq[B]] =
    for {
      ids <- idsFrom(relationships, relationshipType)

      blocks = ids.flatMap(lookup.get)
    } yield blocks

  def lookupOrFail[B](
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

  def idsFrom(
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

  final case class PartialBlockNotFoundException(badBlockId: BlockId) extends ProductException

  final case class BlockNotFoundException(
    badBlockId: BlockId,
    foundAs: Option[sdk.BlockType],
  ) extends ProductException

  def enrichAnyBlockNotFoundFailures[A](allBlocks: ArraySeq[sdk.Block], value: ExceptionOr[A]): ExceptionOr[A] =
    value.left.map(enrichBadBlockException(allBlocks, _))

  private def enrichBadBlockException(allBlocks: ArraySeq[sdk.Block], e: Exception): Exception =
    e match {
      case PartialBlockNotFoundException(badBlockId) =>
        BlockNotFoundException(
          badBlockId,
          foundAs = allBlocks.collectFirst {
            case b if BlockId.fromString(b.id).contains(badBlockId) => b.blockType
          },
        )
      case e @ GenericException(_, Some(cause: Exception)) =>
          e.copy(cause = Some(enrichBadBlockException(allBlocks, cause)))
      case e => e
    }

}
