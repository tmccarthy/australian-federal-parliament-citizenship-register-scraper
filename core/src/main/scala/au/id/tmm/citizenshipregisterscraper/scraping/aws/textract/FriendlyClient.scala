package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.FriendlyClient.{Document, DocumentContent}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.AnalysisResult
import au.id.tmm.citizenshipregisterscraper.scraping.aws.{S3Key, toIO}
import au.id.tmm.digest4s.binarycodecs.syntax._
import au.id.tmm.digest4s.digest.syntax._
import au.id.tmm.digest4s.digest.{MD5Digest, SHA512Digest}
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import cats.effect.{IO, Timer}
import software.amazon.awssdk.core.async.AsyncRequestBody
import software.amazon.awssdk.core.client.config.{ClientAsyncConfiguration, SdkAdvancedAsyncClientOption}
import software.amazon.awssdk.services.s3.S3AsyncClient
import software.amazon.awssdk.services.s3.model.PutObjectRequest
import software.amazon.awssdk.services.textract.model.{DocumentLocation, OutputConfig, S3Object}
import sttp.client3._
import sttp.model.{HeaderNames, Uri => SttpUri}

import java.net.URI
import java.nio.file.{Files, Path}
import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContextExecutor

final class FriendlyClient(
  cache: KeyValueStore,
  s3Bucket: String,
  s3WorkingDirectoryPrefix: S3Key,
  httpClient: SttpBackend[IO, Any],
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

  def runAnalysisFor(documentLocation: FriendlyClient.Document): IO[AnalysisResult] =
    for {
      content <- contentOf(documentLocation)
      possibleJobId <- cache.get[SHA512Digest, TextractJobId](content.sha512Digest)
      result <- possibleJobId match {
        case Some(jobId) => retrieveJobIdResult(jobId)
        case None =>
          for {
            analysisResult <- runAnalysis(content)
            _ <- cache.put(content.sha512Digest, analysisResult.jobId)
          } yield analysisResult
      }
    } yield result

  private def contentOf(document: Document): IO[DocumentContent] =
    document match {
      case local: Document.Local => contentOf(local)
      case remote: Document.Remote => contentOf(remote)
    }

  private def contentOf(localDocument: Document.Local): IO[DocumentContent] =
    IO(Files.readAllBytes(localDocument.path)).map { bytes =>
      DocumentContent(
        localDocument.path.getFileName.toString,
        new ArraySeq.ofByte(bytes),
        contentType = None,
        bytes.sha512,
        bytes.md5,
      )
    }
  private def contentOf(remoteDocument: Document.Remote): IO[DocumentContent] =
    for {
      response: Response[Either[String, Array[Byte]]] <-
        basicRequest
          .response(asByteArray)
          .get(SttpUri(remoteDocument.uri))
          .send(httpClient)

      contentType: Option[String] = response.header(HeaderNames.ContentType)

      bytes <- IO.fromEither {
        response.body
          .map(bytes => new ArraySeq.ofByte(bytes))
          .left
          .map(errorResponse =>
            GenericException(s"Http response code was ${response.code.code}, message was $errorResponse"),
          )
      }

      fileName <- IO.fromEither {
        ExceptionOr.catchIn {
          Path.of(remoteDocument.uri.getPath).getFileName.toString
        }
      }

    } yield DocumentContent(
      fileName,
      bytes,
      contentType,
      bytes.sha512,
      bytes.md5,
    )


  private def retrieveJobIdResult(jobId: TextractJobId): IO[AnalysisResult] =
    analysisClient.getAnalysisResult(jobId)

  private def runAnalysis(
    documentContent: DocumentContent,
  ): IO[AnalysisResult] =
    for {
      s3DirectoryForFile <- IO.pure {
        s3WorkingDirectoryPrefix
          .resolve(S3Key(documentContent.sha512Digest.asHexString))
      }

      s3UploadLocation = s3DirectoryForFile
          .resolve(S3Key(documentContent.fileName))

      textractOutputDirectory = s3DirectoryForFile.resolve("textract_output")

      sdkDocumentLocation = DocumentLocation.builder()
        .s3Object(S3Object.builder().bucket(s3Bucket).name(s3UploadLocation.toRaw).build())
        .build()

      sdkOutputLocation = OutputConfig.builder()
        .s3Bucket(s3Bucket)
        .s3Prefix(textractOutputDirectory.toRaw)
        .build()

      _ <- uploadToS3(documentContent, s3UploadLocation)
      result <- analysisClient.run(sdkDocumentLocation, sdkOutputLocation)

    } yield result

  private def uploadToS3(
    documentContent: DocumentContent,
    key: S3Key,
  ) =
    for {
      putRequest <- IO.pure {
        PutObjectRequest
          .builder()
          .bucket(s3Bucket)
          .key(key.toRaw)
          .contentMD5(documentContent.md5Digest.asBase64String)
          .build()
      }

      putRequestBody = AsyncRequestBody.fromBytes(documentContent.body.unsafeArray)

      _ <- toIO(s3Client.putObject(putRequest, putRequestBody))
    } yield ()


  def close: IO[Unit] =
    for {
      s3CloseResult <- IO(s3Client.close()).attempt
      textractCloseResult <- analysisClient.close.attempt
      result <- IO.fromEither(s3CloseResult orElse textractCloseResult)
    } yield result

}

object FriendlyClient {
  sealed trait Document

  object Document {
    final case class Local(path: Path)  extends Document
    final case class Remote(uri: URI) extends Document

    object Local {
      def apply(path: => Path): IO[Local] = IO(new Local(path))
    }
  }

  final case class DocumentContent(
    fileName: String,
    body: ArraySeq.ofByte,
    contentType: Option[String],
    sha512Digest: SHA512Digest,
    md5Digest: MD5Digest,
  )
}