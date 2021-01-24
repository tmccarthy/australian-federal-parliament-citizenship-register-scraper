package au.id.tmm.citizenshipregisterscraper.mainclasses

import au.id.tmm.citizenshipregisterscraper.documents.senate
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.FriendlyClient
import cats.effect.IO
import sttp.client3.SttpBackend

import scala.collection.immutable.ArraySeq

object TestSenateStatementInRelationToCitizenshipScraping extends MuckingWithDynamoMain {
  protected val abetzDisclosure: IO[senate.DocumentReference] = disclosureForName("Abetz")

  override def runWithDynamo(
    httpClient: SttpBackend[IO, Any],
    jobIdCache: FriendlyClient.JobIdCache,
    friendlyClient: FriendlyClient,
  ): IO[Unit] =
    for {
      reference <- abetzDisclosure

      analysisResult <- friendlyClient.runAnalysisFor(FriendlyClient.Document.Remote(reference.documentLocation))

      tables = analysisResult.pages.to(ArraySeq).flatMap(_.children).collect {
        case textract.model.Page.Child.OfTable(table) => table
      }

      _ <- IO(tables.foreach(t => println(t.rows)))

      _ <- IO.fromEither(SenateStatementInRelationToCitizenship.fromTextract(analysisResult))

    } yield ()
}
