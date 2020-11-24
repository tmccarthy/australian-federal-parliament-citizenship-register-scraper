package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import scala.collection.immutable.ArraySeq

final case class AnalysisResult(
  pages: ArraySeq[Page],
)
