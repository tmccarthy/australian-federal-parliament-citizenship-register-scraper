package au.id.tmm.citizenshipregisterscraper.mainclasses

import au.id.tmm.citizenshipregisterscraper.documents.senate
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.FriendlyClient
import cats.effect.IO
import sttp.client3.SttpBackend

object TestSenateStatementInRelationToCitizenshipScraping extends MuckingWithDynamoMain {
  protected val abetzDisclosure: IO[senate.DocumentReference] = disclosureForName("Askew")

  override def runWithDynamo(
    httpClient: SttpBackend[IO, Any],
    jobIdCache: FriendlyClient.JobIdCache,
    friendlyClient: FriendlyClient,
  ): IO[Unit] =
    for {
      reference <- abetzDisclosure

      analysisResult <- friendlyClient.runAnalysisFor(FriendlyClient.Document.Remote(reference.documentLocation))

      statement <- IO.fromEither(SenateStatementInRelationToCitizenship.fromTextract(analysisResult))

      _ <- IO(println(statement))

    } yield ()
}
