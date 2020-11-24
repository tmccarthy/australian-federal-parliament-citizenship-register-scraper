package au.id.tmm.citizenshipregisterscraper.scraping.aws

import java.net.URI
import java.util.concurrent.TimeUnit

import au.id.tmm.utilities.errors.syntax._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.TextractClient.logger
import cats.effect.{IO, Timer}
import org.slf4j.{Logger, LoggerFactory}
import software.amazon.awssdk.services.textract.model._
import software.amazon.awssdk.services.textract.{TextractClient => AwsTextractClient}

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.FiniteDuration

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

      model <- IO.fromEither {
        textract.Parse.toModel(documentAnalysisResult)
          .wrapExceptionWithMessage(s"Job id was ${startDocumentAnalysisResult.jobId}")
      }

      _ <- IO {
        val tables = model.pages.to(ArraySeq).flatMap(_.children).collect {
          case textract.model.Page.Child.OfTable(table) => table
        }

        tables.foreach(t => println(t.rows))
      }

    } yield ()

}

object TextractClient {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
}
