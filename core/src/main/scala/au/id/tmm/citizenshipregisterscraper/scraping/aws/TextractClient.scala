package au.id.tmm.citizenshipregisterscraper.scraping.aws

import java.io.InputStream
import java.net.URI
import java.nio.ByteBuffer

import cats.effect.{Bracket, IO}
import org.slf4j.LoggerFactory
import software.amazon.awssdk.core.client.config.{ClientAsyncConfiguration, SdkAdvancedAsyncClientOption}
import software.amazon.awssdk.http.SdkHttpClient
import software.amazon.awssdk.http.apache.ApacheHttpClient
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.textract.TextractAsyncClient
import software.amazon.awssdk.services.textract.model.S3Object

import scala.concurrent.ExecutionContextExecutor

class TextractClient(
  executionContext: ExecutionContextExecutor,
  workingS3: TextractClient.WorkingS3,
) {

  private val textractClient = TextractAsyncClient
    .builder()
    .asyncConfiguration(
      ClientAsyncConfiguration
        .builder()
        .advancedOption(SdkAdvancedAsyncClientOption.FUTURE_COMPLETION_EXECUTOR, executionContext)
        .build(),
    )

  def run(documentLocation: URI): IO[Unit] =
    for {
      _ <- ???
    } yield ???

  private def s3ObjectFor(documentLocation: URI): IO[S3Object] =
    for {
      _ <- ???
    } yield ???

  private def targetS3URIFor(documentLocation: URI): URI = ???


}

object TextractClient {
  private val logger = LoggerFactory.getLogger(getClass)

  final case class WorkingS3(
    bucket: String,
    prefix: String = "/",
  )
}
