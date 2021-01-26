package au.id.tmm.citizenshipregisterscraper.scraping

import java.time.{LocalDate, Year}

import au.id.tmm.ausgeo.State
import au.id.tmm.citizenshipregisterscraper.scraping.ScrapingUtilities._
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship.{
  AncestorDetails,
  GrandparentDetails,
}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.{BlockPredicates, ResultNavigator}
import au.id.tmm.collections.syntax._
import au.id.tmm.utilities.errors.ExceptionOr
import au.id.tmm.utilities.errors.syntax._

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

  def fromTextract(textractAnalysis: AnalysisResult): ExceptionOr[SenateStatementInRelationToCitizenship] = {
    val resultNavigator = ResultNavigator(textractAnalysis)

    import resultNavigator.syntax._

    for {
      pages <- Right(textractAnalysis.pages)

      page1 <- getOrFail(pages, index = 0)

      _ <-
        page1
          .searchRecursivelyUsingPredicate[Line](
            BlockPredicates.lineHasWordsLike("Statement in relation to citizenship"),
          )
          .onlyElementOrException
          .wrapExceptionWithMessage("Couldn't find the title")

      surnameKey <-
        page1
          .searchRecursivelyUsingPredicate[KeyValueSet.Key](BlockPredicates.keyHasWordsLike("Surname"))
          .onlyElementOrException
          .wrapExceptionWithMessage("Couldn't find surname")

      surnameValue <- surnameKey.value

      surname = surnameValue.readableText

      _ = println(surname) // TODO remove

      result = SenateStatementInRelationToCitizenship(
        surname,
        ???,
        ???,
        ???,
        ???,
        ???,
        ???,
        ???,
        ???,
        ???,
        ???,
        ???,
        ???,
        ???,
      )
    } yield result
  }

}
