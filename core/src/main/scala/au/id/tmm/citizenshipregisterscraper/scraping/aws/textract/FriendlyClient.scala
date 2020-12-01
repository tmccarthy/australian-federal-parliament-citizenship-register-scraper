package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract

import java.net.URI
import java.nio.file.{Files, Path}

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.FriendlyClient.Document
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.AnalysisResult
import au.id.tmm.digest4s.digest.SHA512Digest
import au.id.tmm.digest4s.digest.syntax._
import cats.effect.{IO, Timer}
import software.amazon.awssdk.core.client.config.{ClientAsyncConfiguration, SdkAdvancedAsyncClientOption}
import software.amazon.awssdk.services.s3.S3AsyncClient
import software.amazon.awssdk.services.textract.TextractClient

import scala.concurrent.ExecutionContextExecutor

final class FriendlyClient(
  cache: KeyValueStore,
  s3Bucket: String,
  s3WorkingDirectoryPrefix: String,
  executionContext: ExecutionContextExecutor,
)(
  implicit timer: Timer[IO],
) {

  private val s3Client =
    S3AsyncClient
      .builder()
      .asyncConfiguration(
        ClientAsyncConfiguration
          .builder()
          .advancedOption(SdkAdvancedAsyncClientOption.FUTURE_COMPLETION_EXECUTOR, executionContext)
          .build(),
      )
      .build()

  private val analysisClient: AwsTextractAnalysisClient = new AwsTextractAnalysisClient()

  private val textractClient = TextractClient
    .builder()
    .build()

  def runAnalysisFor(documentLocation: FriendlyClient.Document): IO[AnalysisResult] =
    for {
      (localPath, digest) <- localWithHash(documentLocation)
      possibleJobId <- cache.get[SHA512Digest, TextractJobId](digest)
      result <- possibleJobId match {
        case Some(jobId) => retrieveJobIdResult(jobId)
        case None =>
          for {
            analysisResult <- runAnalysis(localPath)
            _ <- cache.put(digest, analysisResult.jobId)
          } yield analysisResult
      }
    } yield result

  private def localWithHash(document: Document): IO[(Path, SHA512Digest)] =
    for {
      localPath <- document match {
        case Document.Local(path) => IO.pure(path)
        case Document.Resource(uri) =>
          for {
            tempFile <- IO(Files.createTempFile(getClass.getSimpleName, ""))
            _ <- IO(Files.copy(uri.toURL.openStream(), tempFile))
          } yield tempFile
      }
      digest <- IO(localPath.sha512OrError).flatMap(IO.fromEither)
    } yield (localPath, digest)

  private def retrieveJobIdResult(jobId: TextractJobId): IO[AnalysisResult] =
    analysisClient.getAnalysisResult(jobId)

  private def runAnalysis(documentPath: Path): IO[AnalysisResult] =
    for {
      _ <- ??? : IO[Nothing]
    } yield ???

  def close(): IO[Unit] =
    for {
      s3CloseResult <- IO(s3Client.close()).attempt
      textractCloseResult <- IO(textractClient.close()).attempt
      result <- IO.fromEither(s3CloseResult orElse textractCloseResult)
    } yield result

}

object FriendlyClient {
  sealed trait Document

  object Document {
    final case class Local(path: Path)  extends Document
    final case class Resource(uri: URI) extends Document

    object Local {
      def apply(path: => Path): IO[Local] = IO(new Local(path))
    }
  }
}