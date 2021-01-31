package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{AnalysisResult, Block, Page, PageNumber}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.{AnalysisResultIndex, Searches}
import au.id.tmm.utilities.errors.ExceptionOr

final class AnalysisResultOps[F[_]] private (
  analysisResult: AnalysisResult,
)(implicit
  index: AnalysisResultIndex,
  F: SyntaxErrorContext[F],
) {
  def recursivelySearch[B2 <: Block](collect: PartialFunction[Block, B2]): F[LazyList[B2]] =
    F.lift(Searches.recursivelySearch(analysisResult)(collect))

  def getPage(pageNumber: PageNumber): F[Page] =
    F.lift(ExceptionOr.catchIn(analysisResult.pages.apply(pageNumber.asInt - 1)))
}

object AnalysisResultOps {
  trait ToAnalysisResultOps {
    implicit def toAnalysisResultOps(
      analysisResult: AnalysisResult,
    )(implicit
      index: AnalysisResultIndex,
    ): AnalysisResultOps[ExceptionOr] =
      new AnalysisResultOps(analysisResult)
  }

  trait ToUnsafeAnalysisResultOps {
    implicit def toUnsafeAnalysisResultOps(
      analysisResult: AnalysisResult,
    )(implicit
      index: AnalysisResultIndex,
    ): AnalysisResultOps[SyntaxErrorContext.Unsafe] =
      new AnalysisResultOps(analysisResult)
  }
}
