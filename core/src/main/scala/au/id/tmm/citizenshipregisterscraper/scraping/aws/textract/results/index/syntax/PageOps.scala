package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Page
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.AnalysisResultIndex
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq

final class PageOps private (page: Page)(implicit index: AnalysisResultIndex) extends BlockCommonOps[Page](page) {
  def siblings: ExceptionOr[ArraySeq[Page]] = index.siblingsOf(page)
}

object PageOps {
  trait ToPageOps {
    implicit def toPageOps(page: Page)(implicit index: AnalysisResultIndex): PageOps =
      new PageOps(page)
  }
}
