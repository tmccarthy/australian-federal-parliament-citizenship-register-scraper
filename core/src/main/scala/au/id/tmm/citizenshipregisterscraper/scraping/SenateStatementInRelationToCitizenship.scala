package au.id.tmm.citizenshipregisterscraper.scraping

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, Year}

import au.id.tmm.ausgeo.State
import au.id.tmm.citizenshipregisterscraper.scraping.ScrapingUtilities._
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship.{AncestorDetails, GrandparentDetails}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.GeometricOrdering._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.{BlockPredicates, ResultNavigator}
import au.id.tmm.collections.syntax.toIterableOps
import au.id.tmm.utilities.errors.syntax._
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}

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

      surname <- getValueFromKey(resultNavigator, PageNumber.`1`, "surname")
      otherNames <- getValueFromKey(resultNavigator, PageNumber.`1`, "other names")
      state <- getValueFromKey(resultNavigator, PageNumber.`1`, "state").flatMap(parseStateFrom)
      placeOfBirth <- getValueFromKey(resultNavigator, PageNumber.`1`, "place of birth")
      citizenshipAtBirth <- getValueFromKey(resultNavigator, PageNumber.`1`, "citizenship held at birth")

      dateOfBirth <- extractDateOfBirthUnderHeading(resultNavigator, PageNumber.`1`, "date of birth")
      dateOfAustralianNaturalisation <- extractDateOfBirthUnderHeading(resultNavigator, PageNumber.`1`, "date of australian naturalisation")

      result = SenateStatementInRelationToCitizenship(
        surname,
        otherNames,
        state,
        placeOfBirth,
        citizenshipAtBirth,
        dateOfBirth,
        Some(dateOfAustralianNaturalisation), // TODO need to support this being absent
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



  private def extractDateOfBirthUnderHeading(resultNavigator: ResultNavigator, page: PageNumber, heading: String): ExceptionOr[LocalDate] =
    for {
      heading <-
        resultNavigator
          .searchAllResults[Line] {
            case l: Line if BlockPredicates.lineHasWordsLike(heading)(l) && l.pageNumber == page => l
          }
          .sorted(byDistanceFrom(PageSide.Top))
          .headOption
          .toRight(GenericException(s"No heading: '$heading'"))

      dateOfBirthDay <-
        getValueFromKey(resultNavigator, page, "day", _.sorted(byDistanceFrom(heading)).headOrException)
      dateOfBirthMonth <-
        getValueFromKey(resultNavigator, page, "month", _.sorted(byDistanceFrom(heading)).headOrException)
      dateOfBirthYear <-
        getValueFromKey(resultNavigator, page, "year", _.sorted(byDistanceFrom(heading)).headOrException)

      dateOfBirth <- ExceptionOr.catchIn(LocalDate.parse(s"$dateOfBirthYear-$dateOfBirthMonth-$dateOfBirthDay", DateTimeFormatter.ofPattern("yyyy-M-d")))
    } yield dateOfBirth

  //TODO make this generally available?
  private def getValueFromKey(
    navigator: ResultNavigator,
    page: PageNumber,
    keyText: String,
    choose: LazyList[KeyValueSet.Key] => ExceptionOr[KeyValueSet.Key] = _.onlyElementOrException,
  ): ExceptionOr[String] = {
    import navigator.syntax._

    for {
      candidateKeys <- Right {
        navigator.searchAllResults[KeyValueSet.Key] {
          case k: KeyValueSet.Key if k.pageNumber == page && BlockPredicates.keyHasWordsLike(keyText)(k) => k
        }
      }

      matchingKey <- choose(candidateKeys)
        .wrapExceptionWithMessage(s"Failed to choose key for '$keyText' from ${candidateKeys.map(_.readableText).mkString(", ")}")

      matchingValue <- matchingKey.value
    } yield matchingValue.readableText
  }

  private def parseStateFrom(rawState: String): ExceptionOr[State] = {
    val cleanedRawState = rawState.replaceAll("""\W""", "")

    State.fromName(cleanedRawState).orElse(State.fromAbbreviation(cleanedRawState)).toRight(GenericException(s"Couldn't parse a state from $rawState"))
  }

}
