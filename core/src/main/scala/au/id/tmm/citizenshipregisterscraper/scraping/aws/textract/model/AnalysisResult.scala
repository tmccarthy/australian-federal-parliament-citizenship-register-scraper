package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import scala.collection.immutable.SortedSet

final case class AnalysisResult(
  pages: SortedSet[Page],
  documentMetadata: AnalysisResult.DocumentMetadata,
)

object AnalysisResult {

  final case class DocumentMetadata(
    numPages: Int,
  )

}