package au.id.tmm.citizenshipregisterscraper.scraping

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, Year}

import au.id.tmm.ausgeo.State
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship.ParentalCoupleDetails
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.BlockPredicates
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.BlockPredicates.keyHasWordsLike
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.GeometricOrdering._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.AnalysisResultIndex
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax._
import au.id.tmm.collections.syntax.toIterableOps
import au.id.tmm.utilities.errors.syntax._
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}

import scala.collection.immutable.ArraySeq

final case class SenateStatementInRelationToCitizenship(
  surname: String,
  otherNames: String,
  state: State,
  placeOfBirth: String,
  citizenshipHeldAtBirth: String,
  dateOfBirth: LocalDate,
  dateOfAustralianNaturalisation: Option[LocalDate],
  parents: ParentalCoupleDetails,
  maternalGrandparents: ParentalCoupleDetails,
  paternalGrandparents: ParentalCoupleDetails,
//  otherFactors: String,
//  stepsTakenToAssureCitizenshipNotInherited: String,
//  everBeenForeignCitizen: Boolean,
)

object SenateStatementInRelationToCitizenship {

  final case class ParentalCoupleDetails(
    mother: AncestorDetails,
    father: AncestorDetails,
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

      private[SenateStatementInRelationToCitizenship] def from(dateValue: DateValue): DateOfBirth = dateValue match {
        case DateValue.Unfilled => Unknown
        case DateValue.Of(localDate) => Known(localDate)
        case DateValue.OfYear(year) => YearOnly(year)
      }
    }

  }

  def fromTextract(textractAnalysis: AnalysisResult): ExceptionOr[SenateStatementInRelationToCitizenship] =
    ExceptionOr.flatCatch {
      implicit val index: AnalysisResultIndex = AnalysisResultIndex(textractAnalysis)

      for {
        page1 <- textractAnalysis.getPage(PageNumber.`1`)

        _ <-
          page1
            .recursivelySearchWithPredicate[Line](
              BlockPredicates.lineHasWordsLike("Statement in relation to citizenship"),
            )
            .flatMap(_.onlyElementOrException)
            .wrapExceptionWithMessage("Couldn't find the title")

        surname      <- page1.keysMatching(keyHasWordsLike("surname")).onlyElementOrException.flatMap(_.value).map(_.readableText)
        otherNames   <- page1.keysMatching(keyHasWordsLike("other names")).onlyElementOrException.flatMap(_.value).map(_.readableText)
        placeOfBirth <- page1.keysMatching(keyHasWordsLike("place of birth")).onlyElementOrException.flatMap(_.value).map(_.readableText)
        citizenshipAtBirth <-
          page1.keysMatching(keyHasWordsLike("citizenship held at birth")).onlyElementOrException.flatMap(_.value).map(_.readableText)
        state <-
          page1
            .keysMatching(keyHasWordsLike("state"))
            .onlyElementOrException
            .flatMap(_.value)
            .map(_.readableText)
            .flatMap(parseStateFrom)

        dateOfBirth                    <- extractDateUnderHeading(page1, "date of birth").flatMap {
          case DateValue.Of(localDate) => Right(localDate)
          case d => Left(GenericException(s"Date of birth incomplete $d"))
        }
        dateOfAustralianNaturalisation <- extractDateUnderHeading(page1, "date of australian naturalisation").map {
          case DateValue.Of(localDate) => Some(localDate)
          case DateValue.OfYear(year) => Some(year.atDay(1))
          case DateValue.Unfilled => None
        }

        headingForParentsTable <-
          page1
            .recursivelySearchWithPredicate[Line](
              BlockPredicates.lineHasWordsLike("section 3a-senators parents birth details"),
            )
            .flatMap(_.onlyElementOrException)
            .wrapExceptionWithMessage("Couldn't find heading for parents details")

        parentsTable <-
          page1
            .recursivelySearchWithPredicate[Table](BlockPredicates.beneath(headingForParentsTable))
            .flatMap(_.onlyElementOrException)
            .wrapExceptionWithMessage("Couldn't find parents details table")

        parentsDetails <- parseParentalCoupleDetails(parentsTable)

        page2 <- textractAnalysis.getPage(PageNumber.`2`)

        headingForGrandparentsTables <-
          page2
            .recursivelySearchWithPredicate[Line](
              BlockPredicates.lineHasWordsLike("grandparents birth details"),
            )
            .flatMap(_.onlyElementOrException)
            .wrapExceptionWithMessage("Couldn't find heading for grandparents details")

        headingForOtherFactors <-
          page2
            .recursivelySearchWithPredicate[Line](
              BlockPredicates.lineHasWordsLike("factors that may be relevant"),
            )
            .flatMap(_.onlyElementOrException)
            .wrapExceptionWithMessage("Couldn't find other factors heading")

        grandparentTables <-
          page2
            .recursivelySearchWithPredicate[Table](
              BlockPredicates.strictlyBetween(headingForGrandparentsTables, headingForOtherFactors),
            )
            .map(_.sorted(byDistanceFrom(PageSide.Top)).toList)

        (maternalTable, paternalTable) <- grandparentTables match {
          case maternalTable :: paternalTable :: Nil => Right((maternalTable, paternalTable))
          case badListOfTables                       => Left(GenericException(s"Expected 2 tables but found ${badListOfTables}"))
        }

        maternalGrandparentsDetails <- parseParentalCoupleDetails(maternalTable)
        paternalGrandparentsDetails <- parseParentalCoupleDetails(paternalTable)

        result = SenateStatementInRelationToCitizenship(
          surname,
          otherNames,
          state,
          placeOfBirth,
          citizenshipAtBirth,
          dateOfBirth,
          dateOfAustralianNaturalisation,
          parentsDetails,
          maternalGrandparentsDetails,
          paternalGrandparentsDetails,
        )
      } yield result
    }

  private def parseStateFrom(rawState: String): ExceptionOr[State] = {
    val cleanedRawState = rawState.replaceAll("""\W""", "")

    State
      .fromName(cleanedRawState)
      .orElse(State.fromAbbreviation(cleanedRawState))
      .toRight(GenericException(s"Couldn't parse a state from $rawState"))
  }

  private def extractDateUnderHeading(
    page: Page,
    heading: String,
  )(implicit
    index: AnalysisResultIndex,
  ): ExceptionOr[DateValue] =
    for {
      candidates <- page.recursivelySearchWithPredicate[Line](BlockPredicates.lineHasWordsLike(heading))

      heading <-
        candidates
          .sorted(byDistanceFrom(PageSide.Top))
          .headOption
          .toRight(GenericException(s"No heading: '$heading'"))

      date <- extractDate(page, _.sorted(byDistanceFrom(heading)).headOrException)
    } yield date

  private def extractDate(
    page: Page,
    choose: ArraySeq[KeyValueSet.Key] => ExceptionOr[KeyValueSet.Key],
  )(implicit
    index: AnalysisResultIndex,
  ): ExceptionOr[DateValue] = {
    def cleanRawDatePart(raw: String): String = raw.replaceAll("\\D", "")

    for {
      dateOfBirthDay <-
        choose(page.keysMatching(keyHasWordsLike("day"))).flatMap(_.value).map(v => cleanRawDatePart(v.readableText))
      dateOfBirthMonth <-
        choose(page.keysMatching(keyHasWordsLike("month"))).flatMap(_.value).map(v => cleanRawDatePart(v.readableText))
      dateOfBirthYear <-
        choose(page.keysMatching(keyHasWordsLike("year"))).flatMap(_.value).map(v => cleanRawDatePart(v.readableText))

      date <- ExceptionOr.flatCatch {
        if (dateOfBirthDay.isBlank && dateOfBirthMonth.isBlank && dateOfBirthYear.isBlank) {
          Right(DateValue.Unfilled)
        } else if (!dateOfBirthYear.isBlank && dateOfBirthMonth.isBlank && dateOfBirthYear.isBlank) {
          Right(DateValue.OfYear(Year.parse(dateOfBirthYear, DateTimeFormatter.ofPattern("yyyy"))))
        } else if (!dateOfBirthYear.isBlank && !dateOfBirthMonth.isBlank && !dateOfBirthYear.isBlank) {
          Right(DateValue.Of(LocalDate.parse(s"$dateOfBirthYear-$dateOfBirthMonth-$dateOfBirthDay", DateTimeFormatter.ofPattern("yyyy-M-d"))))
        } else {
          Left(GenericException(s"Bad dates $dateOfBirthYear-$dateOfBirthMonth-$dateOfBirthDay"))
        }
      }
    } yield date
  }

  private def parseParentalCoupleDetails(
    table: Table,
  )(implicit
    index: AnalysisResultIndex,
  ): ExceptionOr[ParentalCoupleDetails] =
    for {
      page <- table.parent

      femalePlaceOfBirthCell <- table.findCell(2, 2)
      femaleDateOfBirthCell  <- table.findCell(2, 3)

      femaleDateOfBirth <- extractDate(
        page,
        keys => keys.filter(BlockPredicates.strictlyWithin(femaleDateOfBirthCell)).onlyElementOrException,
      )
      femalePlaceOfBirth = femalePlaceOfBirthCell.readableText

      malePlaceOfBirthCell <- table.findCell(3, 2)
      maleDateOfBirthCell  <- table.findCell(3, 3)

      maleDateOfBirth <- extractDate(
        page,
        keys => keys.filter(BlockPredicates.strictlyWithin(maleDateOfBirthCell)).onlyElementOrException,
      )
      malePlaceOfBirth = malePlaceOfBirthCell.readableText
    } yield ParentalCoupleDetails(
      mother = AncestorDetails(
        femalePlaceOfBirth,
        AncestorDetails.DateOfBirth.from(femaleDateOfBirth),
      ),
      father = AncestorDetails(
        malePlaceOfBirth,
        AncestorDetails.DateOfBirth.from(maleDateOfBirth),
      ),
    )

  sealed trait DateValue

  object DateValue {
    case object Unfilled extends DateValue
    final case class Of(localDate: LocalDate) extends DateValue
    final case class OfYear(year: Year) extends DateValue
  }

}
