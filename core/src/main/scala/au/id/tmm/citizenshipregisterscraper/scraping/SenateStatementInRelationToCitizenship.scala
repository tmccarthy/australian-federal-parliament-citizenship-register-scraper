package au.id.tmm.citizenshipregisterscraper.scraping

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, Year}

import au.id.tmm.ausgeo.State
import au.id.tmm.citizenshipregisterscraper.scraping.SenateStatementInRelationToCitizenship.ParentalCoupleDetails
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.BlockPredicates.keyHasWordsLike
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.GeometricOrdering._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.AnalysisResultIndex
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax._
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.{BlockPredicates, ChooseBlock}
import au.id.tmm.collections.syntax.toIterableOps
import au.id.tmm.utilities.errors.syntax._
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import cats.syntax.traverse.toTraverseOps

import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

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

      date <- DateValue.fromKVSetsOn(page, ChooseBlock.firstBy(byDistanceFrom(heading)))
    } yield date

  private def parseParentalCoupleDetails(
    table: Table,
  )(implicit
    index: AnalysisResultIndex,
  ): ExceptionOr[ParentalCoupleDetails] =
    for {
      page <- table.parent

      femalePlaceOfBirthCell <- table.findCell(2, 2)
      femaleDateOfBirthCell  <- table.findCell(2, 3)

      femaleDateOfBirth <- DateValue.from(femaleDateOfBirthCell)
      femalePlaceOfBirth = femalePlaceOfBirthCell.readableText

      malePlaceOfBirthCell <- table.findCell(3, 2)
      maleDateOfBirthCell  <- table.findCell(3, 3)

      maleDateOfBirth <- DateValue.from(maleDateOfBirthCell)
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

    def from(cell: Table.Cell)(implicit
      index: AnalysisResultIndex,
    ): ExceptionOr[DateValue] =
      for {
        page <- cell.parent.flatMap(_.parent)
        date <- fromKVSetsOn(page, ChooseBlock.onlyMatching(BlockPredicates.strictlyWithin(cell))) orElse from(cell.readableText)
      } yield date

    def from(text: String): ExceptionOr[DateValue] = ExceptionOr.flatCatch {
      def fullDate = onlyMatchFor("""(\d{1,2})\s+/\s+(\d{1,2})\s+/\s+(\d{4})""".r, text).flatMap {
        case ArraySeq(day, month, year) => Right(Of(LocalDate.of(year, month, day)))
        case bad => Left(GenericException(s"Bad date fields $bad"))
      }

      def fullDateSmallYear = onlyMatchFor("""(\d{1,2})\s+/\s+(\d{1,2})\s+/\s+(\d{2})""".r, text).flatMap {
        case ArraySeq(day, month, year) => Right(Of(LocalDate.of(year + 1900, month, day)))
        case bad => Left(GenericException(s"Bad date fields $bad"))
      }

      def year = onlyMatchFor("""/\s+/\s+(\d{4})""".r, text).flatMap {
        case ArraySeq(year) => Right(OfYear(Year.of(year)))
        case bad => Left(GenericException(s"Bad year fields $bad"))
      }

      def empty = onlyMatchFor("""/\s+/""".r, text).flatMap {
        case ArraySeq() => Right(Unfilled)
        case bad => Left(GenericException(s"Bad year fields $bad"))
      }

      (fullDate orElse fullDateSmallYear orElse year orElse empty)
        .wrapExceptionWithMessage(s"For $text")
    }

    private def onlyMatchFor(pattern: Regex, string: String): ExceptionOr[ArraySeq[Int]] = {
      pattern.findAllMatchIn(string)
        .map { regexMatch =>
          regexMatch.subgroups.to(ArraySeq)
        }
        .to(ArraySeq)
        .onlyElementOrException
        .flatMap { strings =>
          strings.traverse { s =>
            ExceptionOr.catchIn(s.toInt)
          }
        }
    }

    def fromKVSetsOn(page: Page, choose: ChooseBlock[KeyValueSet.Key])(implicit
      index: AnalysisResultIndex,
    ): ExceptionOr[DateValue] =
      for {
        dayBlock <-
          choose.chooseFrom(page.keysMatching(keyHasWordsLike("day"))).flatMap(_.value)
        monthBlock <-
          choose.chooseFrom(page.keysMatching(keyHasWordsLike("month"))).flatMap(_.value)
        yearBlock <-
          choose.chooseFrom(page.keysMatching(keyHasWordsLike("year"))).flatMap(_.value)

        date <- fromBlocks(yearBlock, monthBlock, dayBlock)
      } yield date

    def fromBlocks(
      yearBlock: ReadableText,
      monthBlock: ReadableText,
      dayBlock: ReadableText,
    ): ExceptionOr[DateValue] = ExceptionOr.flatCatch {
      val rawYear = cleanRawDatePart(yearBlock.readableText)
      val rawMonth = cleanRawDatePart(monthBlock.readableText)
      val rawDay = cleanRawDatePart(dayBlock.readableText)

      if (rawDay.isBlank && rawMonth.isBlank && rawYear.isBlank) {
        Right(DateValue.Unfilled)
      } else if (!rawYear.isBlank && rawMonth.isBlank && rawYear.isBlank) {
        Right(DateValue.OfYear(Year.parse(rawYear, DateTimeFormatter.ofPattern("yyyy"))))
      } else if (!rawYear.isBlank && !rawMonth.isBlank && !rawYear.isBlank) {
        Right(DateValue.Of(LocalDate.parse(s"$rawYear-$rawMonth-$rawDay", DateTimeFormatter.ofPattern("yyyy-M-d"))))
      } else {
        Left(GenericException(s"Bad dates $rawYear-$rawMonth-$rawDay"))
      }
    }

    private def cleanRawDatePart(raw: String): String = raw.replaceAll("\\D", "")
  }

}
