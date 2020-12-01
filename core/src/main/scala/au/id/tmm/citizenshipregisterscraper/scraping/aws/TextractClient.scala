package au.id.tmm.citizenshipregisterscraper.scraping.aws

import java.net.URI

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.AwsTextractAnalysisClient
import cats.effect.{IO, Timer}
import software.amazon.awssdk.services.textract.model._

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContextExecutor

class TextractClient(
  executionContext: ExecutionContextExecutor,
  s3WorkingEnvironment: S3WorkingEnvironment,
)(implicit
  timer: Timer[IO],
) {

  private val client = new AwsTextractAnalysisClient()

  def run(documentLocation: URI): IO[Unit] =
    for {
      S3WorkingEnvironment.S3ObjectRef(bucket, key) <- s3WorkingEnvironment.s3ObjectCopyFor(documentLocation)

      analysisResult <- client.run(
        input = DocumentLocation
          .builder()
          .s3Object(
            S3Object
              .builder()
              .bucket(bucket.asString)
              .name(key.toRaw)
              .build(),
          )
          .build(),
        output = OutputConfig
          .builder()
          .s3Bucket(bucket.asString)
          .s3Prefix(S3WorkingEnvironment.S3Key("textract_output").resolve(s3WorkingEnvironment.namePrefix).toRaw)
          .build(),
      )

      _ <- IO {
        val tables = analysisResult.pages.to(ArraySeq).flatMap(_.children).collect {
          case textract.model.Page.Child.OfTable(table) => table
        }

        tables.foreach(t => println(t.rows))
      }

    } yield ()

}
