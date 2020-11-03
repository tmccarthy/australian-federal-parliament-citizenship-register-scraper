package au.id.tmm.citizenshipregisterscraper.scraping.aws

import java.net.URI
import java.util.concurrent.TimeUnit

import cats.effect.{IO, Timer}
import software.amazon.awssdk.core.client.config.{ClientAsyncConfiguration, SdkAdvancedAsyncClientOption}
import software.amazon.awssdk.services.textract.TextractAsyncClient
import software.amazon.awssdk.services.textract.model._

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

class TextractClient(
  executionContext: ExecutionContextExecutor,
  s3WorkingEnvironment: S3WorkingEnvironment,
)(
  implicit timer: Timer[IO],
) {

  private val textractClient = TextractAsyncClient
    .builder()
    .asyncConfiguration(
      ClientAsyncConfiguration
        .builder()
        .advancedOption(SdkAdvancedAsyncClientOption.FUTURE_COMPLETION_EXECUTOR, executionContext)
        .build(),
    )
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

      startDocumentAnalysisResult <- toIO(textractClient.startDocumentAnalysis(startAnalysisRequest))

      getAnalysisRequest =
        GetDocumentAnalysisRequest
          .builder()
          .jobId(startDocumentAnalysisResult.jobId())
          .build()

      _ <- IO.sleep(FiniteDuration(10, TimeUnit.SECONDS))

      documentAnalysisResult <- toIO(textractClient.getDocumentAnalysis(getAnalysisRequest))

      _ <- IO {
        documentAnalysisResult.blocks().asScala.filter(_.blockType() == BlockType.TABLE).foreach { table =>
          println(table.text())
        }
      }

    } yield ()

}

object TextractClient {
}
