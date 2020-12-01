package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.TextractJobId

import scala.collection.immutable.ArraySeq

final case class AnalysisResult(
  jobId: TextractJobId,
  pages: ArraySeq[Page],
)
