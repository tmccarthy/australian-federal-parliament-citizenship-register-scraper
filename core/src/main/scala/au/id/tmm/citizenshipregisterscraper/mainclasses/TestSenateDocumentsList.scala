package au.id.tmm.citizenshipregisterscraper.mainclasses

import java.net.URI

import au.id.tmm.citizenshipregisterscraper.documents.senate.{DocumentReference, ListDocuments}
import cats.effect.{ExitCode, IO, IOApp}

object TestSenateDocumentsList extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      page <- IO(
        new URI(
          "https://www.aph.gov.au/Parliamentary_Business/Committees/Senate/Senators_Interests/RegisterQual46thparl",
        ),
      )
      list <- ListDocuments.from(page)
      _    <- IO {
        list.foreach {
          case DocumentReference(state, senatorName, lastUpdated, documentLocation) => println(List(senatorName, state, lastUpdated, documentLocation).mkString("\t"))
        }
      }
    } yield ExitCode.Success
}
