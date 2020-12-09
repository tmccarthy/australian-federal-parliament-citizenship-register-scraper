package au.id.tmm.citizenshipregisterscraper.mainclasses

import au.id.tmm.citizenshipregisterscraper.documents.senate
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.FriendlyClient
import au.id.tmm.citizenshipregisterscraper.scraping.aws.{DynamoKeyValueStore, S3Key, textract}
import au.id.tmm.utilities.errors.GenericException
import cats.Eval
import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend

import java.net.URI
import java.util.concurrent.Executors
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object TestSenateStatementInRelationToCitizenshipScraping extends IOApp {

  private def disclosureForName(name: String): IO[senate.DocumentReference] =
    for {
      page <- IO(
        new URI(
          "https://www.aph.gov.au/Parliamentary_Business/Committees/Senate/Senators_Interests/RegisterQual46thparl",
        ),
      )
      list <- senate.ListDocuments.from(page)
      documentRef <-
        IO.fromOption(list.find(d => d.senatorName.contains(name)))(GenericException(s"No disclosure for $name"))
    } yield documentRef

  private val abetzDisclosure: IO[senate.DocumentReference] = disclosureForName("Abetz")

  override def run(args: List[String]): IO[ExitCode] =
    for {
      reference  <- abetzDisclosure

      _ <- AsyncHttpClientCatsBackend.resource[IO]().use { httpClient =>
        DynamoKeyValueStore(getClass.getCanonicalName.replace("$", "") + ".cache").use { keyValueStore =>
          FriendlyClient(
            cache = keyValueStore,
            s3Bucket = "au.id.tmm.temp",
            s3WorkingDirectoryPrefix = S3Key("australian-federal-parliament-citizenship-register-scraper", "working"),
            httpClient,
            executionContext.value,
          ).use { friendlyClient =>
            friendlyClient.runAnalysisFor(FriendlyClient.Document.Remote(reference.documentLocation)).flatMap { analysisResult =>
              val tables = analysisResult.pages.to(ArraySeq).flatMap(_.children).collect {
                case textract.model.Page.Child.OfTable(table) => table
              }

              IO(tables.foreach(t => println(t.rows)))
            }
          }
        }
      }
    } yield ExitCode.Success

  private val executionContext: Eval[ExecutionContextExecutorService] =
    Eval.later(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4)))

  override protected implicit def contextShift: ContextShift[IO] = IO.contextShift(executionContext.value)
}
