package au.id.tmm.citizenshipregisterscraper.documents.senate

import java.net.URI
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import au.id.tmm.ausgeo.State
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import cats.effect.IO
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.jdk.CollectionConverters._
import cats.syntax.traverse._

import scala.collection.immutable.ArraySeq

object ListDocuments {

  private val SENATOR_FOR_STATE_PATTERN = {
    val stateNames = State.allStates.map(state => s"(?:${state.name})").mkString("|")

    s"""Senator for ($stateNames)""".r.unanchored
  }

  private val LAST_UPDATED_PATTERN = """Last updated (\d{1,2} \w+ \d{4})""".r.unanchored
  private val LAST_UPDATED_FORMATTER = DateTimeFormatter.ofPattern("d MMMM uuuu")

  def from(aphPage: URI): IO[ArraySeq[DocumentReference]] =
    for {
      document   <- IO(Jsoup.connect(aphPage.toString).get())
      referencesWithRelativePaths <- IO.fromEither(fromJsoupDoc(document))
      referencesWithAbsolutePaths <- referencesWithRelativePaths.traverse { documentReference =>
        for {
          absoluteDocumentLocation <- IO.fromEither(ExceptionOr.catchIn(aphPage.resolve(documentReference.documentLocation)))
        } yield documentReference.copy(documentLocation = absoluteDocumentLocation)
      }
    } yield referencesWithAbsolutePaths

  private def fromJsoupDoc(document: Document): ExceptionOr[ArraySeq[DocumentReference]] =
    for {
      tableBody <- onlyOrFail(document.select(".medium-12 > table:nth-child(3)"))
      tableRows <- failIfEmpty(tableBody.select("tr"))
      docRefs <- tableRows.asScala.to(ArraySeq).traverse(docRefFromTableRow)
    } yield docRefs

  private def docRefFromTableRow(tableRow: Element): ExceptionOr[DocumentReference] =
    for {
      listItem <- onlyOrFail(tableRow.select("td").select("ul").select("li"))
      link <- onlyOrFail(listItem.select("a"))
      name <- failIfEmpty(link.text())
      rawDocumentLocation <- failIfEmpty(link.attr("href"))
      documentLocation <- ExceptionOr.catchIn(new URI(rawDocumentLocation))
      listItemText = listItem.text()
      state <- listItemText match {
        case SENATOR_FOR_STATE_PATTERN("New South Wales") => Right(State.NSW)
        case SENATOR_FOR_STATE_PATTERN("Victoria") => Right(State.VIC)
        case SENATOR_FOR_STATE_PATTERN("Queensland") => Right(State.QLD)
        case SENATOR_FOR_STATE_PATTERN("Western Australia") => Right(State.WA)
        case SENATOR_FOR_STATE_PATTERN("South Australia") => Right(State.SA)
        case SENATOR_FOR_STATE_PATTERN("Tasmania") => Right(State.TAS)
        case SENATOR_FOR_STATE_PATTERN("Northern Territory") => Right(State.NT)
        case SENATOR_FOR_STATE_PATTERN("Australian Capital Territory") => Right(State.ACT)
        case _ => Left(GenericException("Couldn't extract state"))
      }
      lastUpdatedDateString <- listItemText match {
        case LAST_UPDATED_PATTERN(lastUpdatedString) => Right(lastUpdatedString)
        case _ => Left(GenericException("Couldn't extract last updated date"))
      }
      lastUpdatedDate <- ExceptionOr.catchIn(LocalDate.parse(lastUpdatedDateString, LAST_UPDATED_FORMATTER))
    } yield DocumentReference(
      state,
      name,
      lastUpdatedDate,
      documentLocation,
    )

  private def failIfEmpty(elements: Elements): ExceptionOr[Elements] =
    if(!elements.isEmpty) Right(elements) else Left(GenericException("No elements"))

  private def failIfEmpty(string: String): ExceptionOr[String] =
    if(!string.isEmpty) Right(string) else Left(GenericException("Empty string"))

  private def onlyOrFail(elements: Elements): ExceptionOr[Element] =
    if(elements.size == 1) Right(elements.first()) else Left(GenericException(s"Expected 1, but was ${elements.size()}"))

}
