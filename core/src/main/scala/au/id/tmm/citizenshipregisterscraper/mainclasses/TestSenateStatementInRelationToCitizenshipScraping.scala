package au.id.tmm.citizenshipregisterscraper.mainclasses

import java.net.URI
import java.util.concurrent.Executors

import au.id.tmm.citizenshipregisterscraper.documents.senate
import au.id.tmm.citizenshipregisterscraper.scraping.aws.TextractClient
import au.id.tmm.utilities.errors.GenericException
import cats.Eval
import cats.effect.{ContextShift, ExitCode, IO, IOApp}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object TestSenateStatementInRelationToCitizenshipScraping extends IOApp {

  private def disclosureForName(name: String): IO[senate.DocumentReference] =
    for {
      page <- IO(new URI("https://www.aph.gov.au/Parliamentary_Business/Committees/Senate/Senators_Interests/RegisterQual46thparl"))
      list <- senate.ListDocuments.from(page)
      documentRef <- IO.fromOption(list.find(d => d.senatorName.contains(name)))(GenericException(s"No disclosure for $name"))
    } yield documentRef

  private val abetzDisclosure: IO[senate.DocumentReference] = disclosureForName("Abetz")

  override def run(args: List[String]): IO[ExitCode] =
    for {
      reference <- abetzDisclosure
      client = new TextractClient(executionContext.value)
      result <- client.run(reference.documentLocation)
    } yield ExitCode.Success

  private val executionContext: Eval[ExecutionContextExecutorService] =
    Eval.later(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2)))

  override protected implicit def contextShift: ContextShift[IO] = IO.contextShift(executionContext.value)
}
