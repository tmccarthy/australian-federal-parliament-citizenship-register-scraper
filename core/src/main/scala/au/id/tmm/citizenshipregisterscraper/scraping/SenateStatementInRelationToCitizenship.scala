package au.id.tmm.citizenshipregisterscraper.scraping

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, Year}

import au.id.tmm.ausgeo.State
import au.id.tmm.citizenshipregisterscraper.scraping.ScrapingUtilities._
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship.ParentalCoupleDetails
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.BlockPredicates
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.GeometricOrdering._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.{AnalysisResultIndex, Searches}
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

    }

  }

  def fromTextract(textractAnalysis: AnalysisResult): ExceptionOr[SenateStatementInRelationToCitizenship] = {
    implicit val index: AnalysisResultIndex = AnalysisResultIndex(textractAnalysis)

    for {
      pages <- Right(textractAnalysis.pages)

      page1 <- getOrFail(pages, index = 0)

      _ <-
        page1
          .recursivelySearchWithPredicate[Line](
            BlockPredicates.lineHasWordsLike("Statement in relation to citizenship"),
          )
          .onlyElementOrException
          .wrapExceptionWithMessage("Couldn't find the title")

      surname            <- getValueFromKey(PageNumber.`1`, "surname")
      otherNames         <- getValueFromKey(PageNumber.`1`, "other names")
      state              <- getValueFromKey(PageNumber.`1`, "state").flatMap(parseStateFrom)
      placeOfBirth       <- getValueFromKey(PageNumber.`1`, "place of birth")
      citizenshipAtBirth <- getValueFromKey(PageNumber.`1`, "citizenship held at birth")

      dateOfBirth <- extractDateUnderHeading(PageNumber.`1`, "date of birth")
      dateOfAustralianNaturalisation <-
        extractDateUnderHeading(PageNumber.`1`, "date of australian naturalisation")

      headingForParentsTable <-
        page1
          .recursivelySearchWithPredicate[Line](
            BlockPredicates.lineHasWordsLike("section 3a-senators parents birth details"),
          )
          .onlyElementOrException
          .wrapExceptionWithMessage("Couldn't find heading for parents details")

      parentsTable <-
        page1
          .recursivelySearchWithPredicate[Table](BlockPredicates.beneath(headingForParentsTable))
          .onlyElementOrException
          .wrapExceptionWithMessage("Couldn't find parents details table")

      parentsDetails <- parseParentalCoupleDetails(parentsTable)

      page2 <- getOrFail(pages, index = 1)

      headingForGrandparentsTables <-
        page2
          .recursivelySearchWithPredicate[Line](
            BlockPredicates.lineHasWordsLike("grandparents birth details"),
          )
          .onlyElementOrException
          .wrapExceptionWithMessage("Couldn't find heading for grandparents details")

      headingForOtherFactors <-
        page2
          .recursivelySearchWithPredicate[Line](
            BlockPredicates.lineHasWordsLike("factors that may be relevant"),
          )
          .onlyElementOrException
          .wrapExceptionWithMessage("Couldn't find other factors heading")

      grandparentTables =
        page2
          .recursivelySearchWithPredicate[Table](
            BlockPredicates.between(headingForGrandparentsTables, headingForOtherFactors),
          )
          .sorted(byDistanceFrom(PageSide.Top))
          .toList

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
        Some(dateOfAustralianNaturalisation), // TODO need to support this being absent
        parentsDetails,
        maternalGrandparentsDetails,
        paternalGrandparentsDetails,
      )
    } yield result
  }

  //TODO make this generally available?
  private def getValueFromKey(
    page: PageNumber,
    keyText: String,
    choose: LazyList[KeyValueSet.Key] => ExceptionOr[KeyValueSet.Key] = _.onlyElementOrException,
  )(implicit
    index: AnalysisResultIndex,
  ): ExceptionOr[String] = {
    for {
      candidateKeys <- Right {
        Searches.recursivelySearch[KeyValueSet.Key] {
          case k: KeyValueSet.Key if k.pageNumber == page && BlockPredicates.keyHasWordsLike(keyText)(k) => k
        }
      }

      matchingKey <- choose(candidateKeys)
        .wrapExceptionWithMessage(
          s"Failed to choose key for '$keyText' from ${candidateKeys.map(_.readableText).mkString(", ")}",
        )

      matchingValue <- matchingKey.value
    } yield matchingValue.readableText
  }

  private def parseStateFrom(rawState: String): ExceptionOr[State] = {
    val cleanedRawState = rawState.replaceAll("""\W""", "")

    State
      .fromName(cleanedRawState)
      .orElse(State.fromAbbreviation(cleanedRawState))
      .toRight(GenericException(s"Couldn't parse a state from $rawState"))
  }

  private def extractDateUnderHeading(
    page: PageNumber,
    heading: String,
  )(implicit
    index: AnalysisResultIndex,
  ): ExceptionOr[LocalDate] =
    for {
      heading <-
        Searches.recursivelySearch[Line] {
            case l: Line if BlockPredicates.lineHasWordsLike(heading)(l) && l.pageNumber == page => l
          }
          .sorted(byDistanceFrom(PageSide.Top))
          .headOption
          .toRight(GenericException(s"No heading: '$heading'"))

      date <- extractDate(page, _.sorted(byDistanceFrom(heading)).headOrException)
    } yield date

  private def extractDate(
    page: PageNumber,
    choose: LazyList[KeyValueSet.Key] => ExceptionOr[KeyValueSet.Key],
  )(implicit
    index: AnalysisResultIndex,
  ): ExceptionOr[LocalDate] = {
    def cleanRawDatePart(raw: String): String = raw.replaceAll("\\D", "")

    for {
      dateOfBirthDay   <- getValueFromKey(page, "day", choose).map(cleanRawDatePart)
      dateOfBirthMonth <- getValueFromKey(page, "month", choose).map(cleanRawDatePart)
      dateOfBirthYear  <- getValueFromKey(page, "year", choose).map(cleanRawDatePart)

      date <- ExceptionOr.catchIn(
        LocalDate.parse(s"$dateOfBirthYear-$dateOfBirthMonth-$dateOfBirthDay", DateTimeFormatter.ofPattern("yyyy-M-d")),
      )
    } yield date
  }

  private def parseParentalCoupleDetails(
    table: Table,
  )(implicit
    index: AnalysisResultIndex,
  ): ExceptionOr[ParentalCoupleDetails] = {
    for {
      femalePlaceOfBirthCell <- table.findCell(2, 2)
      femaleDateOfBirthCell  <- table.findCell(2, 3)

      femaleDateOfBirth <- extractDate(
        femaleDateOfBirthCell.pageNumber,
        keys => keys.filter(BlockPredicates.within(femaleDateOfBirthCell)).onlyElementOrException,
      )
      femalePlaceOfBirth = femalePlaceOfBirthCell.readableText

      malePlaceOfBirthCell <- table.findCell(3, 2)
      maleDateOfBirthCell  <- table.findCell(3, 3)

      maleDateOfBirth <- extractDate(
        maleDateOfBirthCell.pageNumber,
        keys => keys.filter(BlockPredicates.within(maleDateOfBirthCell)).onlyElementOrException,
      )
      malePlaceOfBirth = malePlaceOfBirthCell.readableText
    } yield ParentalCoupleDetails(
      mother = AncestorDetails(
        femalePlaceOfBirth,
        AncestorDetails.DateOfBirth.Known(femaleDateOfBirth), // TODO support the other forms
      ),
      father = AncestorDetails(
        malePlaceOfBirth,
        AncestorDetails.DateOfBirth.Known(maleDateOfBirth),
      ),
    )
  }

}
