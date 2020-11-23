package au.id.tmm.citizenshipregisterscraper.scraping.aws

import java.net.URI
import java.util.concurrent.TimeUnit

import au.id.tmm.citizenshipregisterscraper.scraping.aws.TextractClient.{ExtractedTable, logger}
import au.id.tmm.collections.NonEmptySet
import au.id.tmm.collections.syntax.toIterableOps
import au.id.tmm.utilities.errors.syntax._
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import au.id.tmm.utilities.syntax.tuples.->
import cats.effect.{IO, Timer}
import cats.syntax.traverse.toTraverseOps
import org.slf4j.{Logger, LoggerFactory}
import software.amazon.awssdk.services.textract.model._
import software.amazon.awssdk.services.textract.{TextractClient => AwsTextractClient}

import scala.collection.immutable.ArraySeq
import scala.collection.{MapView, mutable}
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

class TextractClient(
  executionContext: ExecutionContextExecutor,
  s3WorkingEnvironment: S3WorkingEnvironment,
)(implicit
  timer: Timer[IO],
) {

  private val textractClient = AwsTextractClient
    .builder()
    .build()

  def run(documentLocation: URI): IO[Unit] =
    for {
      S3WorkingEnvironment.S3ObjectRef(bucket, key) <- s3WorkingEnvironment.s3ObjectCopyFor(documentLocation)

      startAnalysisRequest =
        StartDocumentAnalysisRequest
          .builder()
          .documentLocation(
            DocumentLocation
              .builder()
              .s3Object(
                S3Object
                  .builder()
                  .bucket(bucket.asString)
                  .name(key.toRaw)
                  .build(),
              )
              .build(),
          )
          .featureTypes(FeatureType.FORMS, FeatureType.TABLES)
          .outputConfig(
            OutputConfig
              .builder()
              .s3Bucket(bucket.asString)
              .s3Prefix(s3WorkingEnvironment.namePrefix.resolve(S3WorkingEnvironment.S3Key("textract_output")).toRaw)
              .build(),
          )
          .build()

      startDocumentAnalysisResult <- IO(textractClient.startDocumentAnalysis(startAnalysisRequest))

      _ <- IO(logger.info("Sent document analysis request"))

      _ <- IO.sleep(FiniteDuration(1, TimeUnit.MINUTES))

      getAnalysisRequest =
        GetDocumentAnalysisRequest
          .builder()
          .jobId(startDocumentAnalysisResult.jobId())
          .build()

      _ <- IO(logger.info("Getting document analysis"))

      documentAnalysisResult <- IO(textractClient.getDocumentAnalysis(getAnalysisRequest))
      // TODO next token handling

      _ <- IO(logger.info("Received document analysis"))

      tables <- IO.fromEither {
        tablesFrom(documentAnalysisResult)
      }

      _ <- IO {
        tables.foreach(println)
      }

    } yield ()

  private def tablesFrom(
    documentAnalysisResponse: GetDocumentAnalysisResponse,
  ): ExceptionOr[ArraySeq[ExtractedTable]] = {
    val blockPerId: MapView[String, Block] =
      documentAnalysisResponse
        .blocks()
        .asScala
        .groupBy(_.id())
        .view
        .mapValues(_.head)

    val tables = documentAnalysisResponse
      .blocks()
      .asScala
      .collect {
        case block if block.blockType() == BlockType.TABLE => block
      }
      .to(ArraySeq)

    tables.traverse { table =>
      extract(blockPerId, table)
    }
  }

  private def extract(blockPerId: MapView[String, Block], table: Block): ExceptionOr[ExtractedTable] =
    for {
      cellBlocks <- getChildrenFor(blockPerId, table, expectedType = NonEmptySet.of(BlockType.CELL))
      cells <- ExceptionOr.catchIn {
        val cellsPerRowPerColumn: mutable.Map[Int, mutable.Map[Int, String]] = mutable.Map()

        cellBlocks.foreach { block =>
          cellsPerRowPerColumn
            .getOrElseUpdate(block.rowIndex(), mutable.Map())
            .update(block.columnIndex(), textFrom(blockPerId, block).getOrThrow)
        }

        val numRows: Int = cellsPerRowPerColumn.keySet.max
        val numColumns: Int =
          cellsPerRowPerColumn.values.map(_.keySet.max).to(Set).onlyElementOrException.getOrThrow

        (1 to numRows)
          .map { row =>
            (1 to numColumns)
              .map { column =>
                cellsPerRowPerColumn
                  .get(row)
                  .toRight(GenericException(s"Missing row $row"))
                  .getOrThrow
                  .get(column)
                  .toRight(GenericException(s"Missing column $column"))
                  .getOrThrow
              }
              .to(ArraySeq)
          }
          .to(ArraySeq)
      }
    } yield ExtractedTable(cells)

  // TODO these can contain checkboxes etc
  private def textFrom(blockPerId: MapView[String, Block], cell: Block): ExceptionOr[String] =
    for {
      childBlocks <- getChildrenFor(
        blockPerId,
        cell,
        expectedType = NonEmptySet.of(BlockType.WORD, BlockType.LINE, BlockType.SELECTION_ELEMENT),
      )
      words = childBlocks.flatMap(b => Option(b.text()))
    } yield words.mkString(" ")

  private def getChildrenFor(
    blockPerId: MapView[String, Block],
    block: Block,
    expectedType: NonEmptySet[BlockType],
  ): ExceptionOr[ArraySeq[Block]] =
    block.relationships().asScala.to(ArraySeq).flatTraverse {
      case relationship if relationship.`type` == RelationshipType.CHILD =>
        relationship.ids.asScala.to(ArraySeq).traverse { id =>
          blockPerId
            .get(id)
            .toRight(GenericException(s"No block for id $id"))
            .filterOrElse(
              b => expectedType.contains(b.blockType()),
              GenericException(s"Block $id is a child expected a type of $expectedType"),
            )
        }
    }

}

object TextractClient {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  final case class ExtractedTable(
    cells: ArraySeq[ArraySeq[String]],
  )

  final case class ExtractedKeyValueSet(
    pairs: ArraySeq[String -> String],
  )

}
