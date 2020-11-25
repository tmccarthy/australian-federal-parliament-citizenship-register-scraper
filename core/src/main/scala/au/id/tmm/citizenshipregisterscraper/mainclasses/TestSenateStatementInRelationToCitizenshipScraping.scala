package au.id.tmm.citizenshipregisterscraper.mainclasses

import java.net.URI
import java.util.concurrent.Executors

import au.id.tmm.citizenshipregisterscraper.documents.senate
import au.id.tmm.citizenshipregisterscraper.scraping.aws.{S3WorkingEnvironment, TextractClient}
import au.id.tmm.utilities.errors.GenericException
import cats.Eval
import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend

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
      // TODO something here isn't getting released at the end
      reference  <- abetzDisclosure
      httpClient <- AsyncHttpClientCatsBackend[IO]()
      s3WorkingEnvironment = new S3WorkingEnvironment(
        bucket = S3WorkingEnvironment.S3BucketName("au.id.tmm.temp"),
        namePrefix = S3WorkingEnvironment.S3Key(getClass.getCanonicalName.split('.').toList),
        httpClient,
        executionContext.value,
      )
      client = new TextractClient(executionContext.value, s3WorkingEnvironment)
      _ <- client.run(reference.documentLocation)
    } yield ExitCode.Success

  private val executionContext: Eval[ExecutionContextExecutorService] =
    Eval.later(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4)))

  override protected implicit def contextShift: ContextShift[IO] = IO.contextShift(executionContext.value)
}
