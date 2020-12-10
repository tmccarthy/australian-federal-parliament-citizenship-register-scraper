package au.id.tmm.citizenshipregisterscraper.mainclasses

import java.net.URI
import java.util.concurrent.{Executors, TimeUnit}

import au.id.tmm.citizenshipregisterscraper.documents.senate
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.FriendlyClient
import au.id.tmm.citizenshipregisterscraper.scraping.aws.{S3Key, textract}
import au.id.tmm.utilities.errors.GenericException
import cats.effect.{ExitCode, IO, IOApp, Resource, SyncIO}
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend

import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object TestSenateStatementInRelationToCitizenshipScraping extends IOApp.WithContext {

  override protected def executionContextResource: Resource[SyncIO, ExecutionContextExecutorService] =
    Resource
      .make(SyncIO(Executors.newFixedThreadPool(8)))(pool =>
        SyncIO {
          pool.shutdown()
          pool.awaitTermination(10, TimeUnit.SECONDS)
        },
      )
      .map(ExecutionContext.fromExecutorService)

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
      reference <- abetzDisclosure

      _ <- AsyncHttpClientCatsBackend.resource[IO]().use { httpClient =>
        FriendlyClient.JobIdCache.UsingDynamoDb(getClass.getCanonicalName.replace("$", "") + ".cache").use {
          jobIdCache =>
            FriendlyClient(
              cache = jobIdCache,
              s3Bucket = "au.id.tmm.temp",
              s3WorkingDirectoryPrefix = S3Key("australian-federal-parliament-citizenship-register-scraper", "working"),
              httpClient,
              executionContext.asInstanceOf[ExecutionContextExecutorService],
            ).use { friendlyClient =>
              friendlyClient.runAnalysisFor(FriendlyClient.Document.Remote(reference.documentLocation)).flatMap {
                analysisResult =>
                  val tables = analysisResult.pages.to(ArraySeq).flatMap(_.children).collect {
                    case textract.model.Page.Child.OfTable(table) => table
                  }

                  IO(tables.foreach(t => println(t.rows)))
              }
            }
        }
      }
    } yield ExitCode.Success

}
