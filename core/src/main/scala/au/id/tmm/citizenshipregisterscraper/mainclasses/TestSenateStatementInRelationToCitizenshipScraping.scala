package au.id.tmm.citizenshipregisterscraper.mainclasses

import java.net.URI

import au.id.tmm.citizenshipregisterscraper.documents.senate
import au.id.tmm.citizenshipregisterscraper.documents.senate.{DocumentReference, ListDocuments}
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.FriendlyClient
import cats.effect.IO
import org.slf4j.{Logger, LoggerFactory}
import sttp.client3.SttpBackend

object TestSenateStatementInRelationToCitizenshipScraping extends MuckingWithDynamoMain {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private def useDisclosure(documentReference: senate.DocumentReference): Boolean =
    Set(
      "Abetz",
      "Askew",
      "Birmingham",
//      "Canavan", // TODO failing to pickup the key for "place of birth"
//      "Carr", // TODO failing to pickup the key for "citizenship held at birth"
      "Cash",
//      "Chisholm", // TODO failing to pickup the key for "citizenship held at birth"
      "Whish-Wilson",
      "Wong",
    ).exists(documentReference.senatorName.contains)

  override def runWithDynamo(
    httpClient: SttpBackend[IO, Any],
    jobIdCache: FriendlyClient.JobIdCache,
    friendlyClient: FriendlyClient,
  ): IO[Unit] =
    for {
      documentsListUri <- IO(
        new URI(
          "https://www.aph.gov.au/Parliamentary_Business/Committees/Senate/Senators_Interests/RegisterQual46thparl",
        ),
      )

      documents <- ListDocuments.from(documentsListUri)

      _ <-
        fs2.Stream
          .chunk[IO, DocumentReference](fs2.Chunk.arraySeq(documents))
          .filter(useDisclosure)
          .parEvalMap(maxConcurrent = 4) { documentReference =>
            friendlyClient
              .runAnalysisFor(FriendlyClient.Document.Remote(documentReference.documentLocation))
              .flatMap(analysisResult =>
                IO.fromEither(SenateStatementInRelationToCitizenship.fromTextract(analysisResult)),
              )
              .attempt
              .flatMap {
                case Right(parsedStatement) =>
                  IO(logger.info(s"✅ ${documentReference.senatorName} => $parsedStatement"))
                case Left(error) => IO(logger.error(s"❌ Failed for ${documentReference.senatorName}", error))
              }
          }
          .compile
          .drain
    } yield ()
}
