package au.id.tmm.citizenshipregisterscraper.scraping

import java.time.{LocalDate, Year}

import au.id.tmm.ausgeo.State
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship.{
  AncestorDetails,
  GrandparentDetails,
}

final case class SenateStatementInRelationToCitizenship(
  surname: String,
  otherNames: String,
  state: State,
  placeOfBirth: String,
  citizenshipHeldAtBirth: String,
  dateOfBirth: LocalDate,
  dateOfAustralianNaturalisation: Option[LocalDate],
  motherDetails: AncestorDetails,
  fatherDetails: AncestorDetails,
  maternal: GrandparentDetails,
  paternal: GrandparentDetails,
  otherFactors: String,
  stepsTakenToAssureCitizenshipNotInherited: String,
  everBeenForeignCitizen: Boolean,
)

object SenateStatementInRelationToCitizenship {

  final case class GrandparentDetails(
    grandmother: AncestorDetails,
    grandfather: AncestorDetails,
  )

  final case class AncestorDetails(
    placeOfBirth: String,
    dateOfBirth: AncestorDetails.DateOfBirth,
  )

  object AncestorDetails {
    sealed trait DateOfBirth

    object DateOfBirth {
      final case class Known(localDate: LocalDate) extends DateOfBirth
      final case class YearOnly(year: Year)        extends DateOfBirth
      case object Unknown                          extends DateOfBirth
    }
  }

}
