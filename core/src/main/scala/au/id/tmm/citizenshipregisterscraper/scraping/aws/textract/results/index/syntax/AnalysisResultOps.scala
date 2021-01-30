package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{AnalysisResult, Block}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.{AnalysisResultIndex, Searches}
import au.id.tmm.utilities.errors.ExceptionOr

final class AnalysisResultOps private (analysisResult: AnalysisResult)(implicit index: AnalysisResultIndex) {
  def recursivelySearch[B2 <: Block](collect: PartialFunction[Block, B2]): ExceptionOr[LazyList[B2]] =
    Searches.recursivelySearch(analysisResult)(collect)
}

object AnalysisResultOps {
  trait ToAnalysisResultOps {
    implicit def toAnalysisResultOps(analysisResult: AnalysisResult)(implicit index: AnalysisResultIndex): AnalysisResultOps =
      new AnalysisResultOps(analysisResult)
  }
}
