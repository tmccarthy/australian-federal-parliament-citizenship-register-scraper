package au.id.tmm.citizenshipregisterscraper.mainclasses

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.FriendlyClient
import cats.effect.IO
import sttp.client3.SttpBackend

// TODO this isn't working
object ClearCaches extends MuckingWithDynamoMain {
  override def runWithDynamo(httpClient: SttpBackend[IO, Any], jobIdCache: FriendlyClient.JobIdCache, friendlyClient: FriendlyClient): IO[Unit] =
    jobIdCache.clear
}
