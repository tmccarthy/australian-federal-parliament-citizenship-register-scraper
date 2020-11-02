package au.id.tmm.citizenshipregisterscraper.scraping.aws

import java.io.InputStream
import java.net.URI
import java.nio.ByteBuffer

import au.id.tmm.citizenshipregisterscraper.scraping.aws.TextractClient.logger
import cats.Eval
import cats.effect.{Bracket, IO}
import com.amazonaws.handlers.AsyncHandler
import com.amazonaws.services.textract.model._
import com.amazonaws.services.textract.{AmazonTextractAsync, AmazonTextractAsyncClientBuilder}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContextExecutorService

class TextractClient(executionContext: ExecutionContextExecutorService) {

  private val client: Eval[AmazonTextractAsync] = Eval.later(
    AmazonTextractAsyncClientBuilder
      .standard()
      .withExecutorFactory(() => executionContext)
      .build(),
  )

  def run(documentLocation: URI): IO[AnalyzeDocumentResult] =
    for {
      _        <- IO(logger.info(s"Analyse request for $documentLocation"))
      document <- makeDocument(documentLocation)
      request = new AnalyzeDocumentRequest()
        .withDocument(document)
        .withFeatureTypes(FeatureType.TABLES, FeatureType.FORMS)

      result <- runAnalyseRequest(request)
    } yield result

  private def makeDocument(documentLocation: URI): IO[Document] =
    documentLocation.getScheme match {
      case "s3" => {
        val s3Object = new S3Object().withBucket(documentLocation.getHost).withName(documentLocation.getPath)
        IO.pure(new Document().withS3Object(s3Object))
      }
      case _ => streamToBytes(documentLocation).map(bytes => new Document().withBytes(bytes))
    }

  private def streamToBytes(documentLocation: URI): IO[ByteBuffer] =
    Bracket[IO, Throwable].bracket(
      acquire = IO(documentLocation.toURL.openStream()),
    )(
      use = readIntoBytes,
    )(
      release = stream => IO(stream.close()),
    )

  private def readIntoBytes(inputStream: InputStream): IO[ByteBuffer] =
    IO {
      ByteBuffer.wrap(inputStream.readAllBytes())
    }

  private def runAnalyseRequest(request: AnalyzeDocumentRequest): IO[AnalyzeDocumentResult] =
    IO.async[AnalyzeDocumentResult] { callback: (Either[Throwable, AnalyzeDocumentResult] => Unit) =>
      client.value.analyzeDocumentAsync(request, makeAsyncHandlerUsing(callback))
    }

  private def makeAsyncHandlerUsing(
    callback: Either[Throwable, AnalyzeDocumentResult] => Unit,
  ): AsyncHandler[AnalyzeDocumentRequest, AnalyzeDocumentResult] =
    new AsyncHandler[AnalyzeDocumentRequest, AnalyzeDocumentResult] {
      override def onError(exception: Exception): Unit = callback(Left(exception))
      override def onSuccess(request: AnalyzeDocumentRequest, result: AnalyzeDocumentResult): Unit =
        callback(Right(result))
    }

}

object TextractClient {
  private val logger = LoggerFactory.getLogger(getClass)
}
