package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract

import java.time.Duration

import au.id.tmm.citizenshipregisterscraper.scraping.aws.RetryEffect
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.Client.{JobId, logger}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.AnalysisResult
import au.id.tmm.collections.NonEmptyArraySeq
import au.id.tmm.utilities.errors.GenericException
import cats.effect.{IO, Timer}
import org.slf4j.{Logger, LoggerFactory}
import software.amazon.awssdk.services.textract.model.GetDocumentAnalysisRequest
import software.amazon.awssdk.services.{textract => sdk}

import scala.collection.immutable.ArraySeq

class Client(implicit timer: Timer[IO]) {

  private val textractClient = sdk.TextractClient
    .builder()
    .build()

  def run(
    input: sdk.model.DocumentLocation,
    output: sdk.model.OutputConfig,
  ): IO[AnalysisResult] =
    for {
      jobId          <- startAnalysis(input, output)
      responses      <- getAnalysisResult(jobId)
      analysisResult <- IO.fromEither(Parse.toModel(responses.underlying))
    } yield analysisResult

  private def startAnalysis(
    input: sdk.model.DocumentLocation,
    output: sdk.model.OutputConfig,
  ): IO[JobId] =
    for {
      startAnalysisRequest <- IO.pure(makeStartAnalysisRequest(input, output))
      _                    <- IO(logger.info("Sent document analysis request"))
      startAnalysisResult  <- IO(textractClient.startDocumentAnalysis(startAnalysisRequest))
    } yield JobId(startAnalysisResult.jobId)

  private def makeStartAnalysisRequest(
    input: sdk.model.DocumentLocation,
    output: sdk.model.OutputConfig,
  ): sdk.model.StartDocumentAnalysisRequest =
    sdk.model.StartDocumentAnalysisRequest
      .builder()
      .documentLocation(input)
      .featureTypes(
        sdk.model.FeatureType.FORMS,
        sdk.model.FeatureType.TABLES,
      )
      .outputConfig(output)
      .build()

  private def getAnalysisResult(jobId: JobId): IO[NonEmptyArraySeq[sdk.model.GetDocumentAnalysisResponse]] =
    for {
      firstPage  <- waitUntilFinished(jobId)
      otherPages <- readRemaining(jobId, firstPage)
    } yield NonEmptyArraySeq.fromHeadTail(firstPage, otherPages)

  private def waitUntilFinished(jobId: JobId): IO[sdk.model.GetDocumentAnalysisResponse] =
    for {
      getAnalysisRequest <- IO.pure(
        GetDocumentAnalysisRequest
          .builder()
          .jobId(jobId.asString)
          .build(),
      )
      getAnalysisResponse <- RetryEffect.exponentialRetry(
        op = IO {
          textractClient.getDocumentAnalysis(getAnalysisRequest)
        }.flatMap { response =>
          response.jobStatus match {
            case sdk.model.JobStatus.SUCCEEDED   => IO.pure(RetryEffect.Result.Finished(response))
            case sdk.model.JobStatus.IN_PROGRESS => IO.raiseError(GenericException("Job in progress"))
            case sdk.model.JobStatus.FAILED | sdk.model.JobStatus.PARTIAL_SUCCESS |
                sdk.model.JobStatus.UNKNOWN_TO_SDK_VERSION =>
              IO.pure(RetryEffect.Result.FailedFinished(GenericException("Job failed")))
          }
        },
        initialDelay = Duration.ofSeconds(10),
        factor = 1,
        maxWait = Duration.ofMinutes(2),
      )
    } yield getAnalysisResponse

  private def readRemaining(
    jobId: JobId,
    firstResult: sdk.model.GetDocumentAnalysisResponse,
  ): IO[ArraySeq[sdk.model.GetDocumentAnalysisResponse]] = {
    def go(
      nextToken: Option[String],
      responsesSoFar: List[sdk.model.GetDocumentAnalysisResponse],
    ): IO[List[sdk.model.GetDocumentAnalysisResponse]] =
      nextToken match {
        case None => IO.pure(responsesSoFar)
        case Some(nextToken) => {
          val request = GetDocumentAnalysisRequest
            .builder()
            .jobId(jobId.asString)
            .nextToken(nextToken)
            .build()

          for {
            response <- IO(textractClient.getDocumentAnalysis(request))
            nextToken = Option(response.nextToken())
            allResponses <- go(nextToken, responsesSoFar :+ response)
          } yield allResponses
        }
      }

    go(Option(firstResult.nextToken), responsesSoFar = List.empty).map(_.to(ArraySeq))
  }

}

object Client {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private case class JobId(asString: String) extends AnyVal
}
