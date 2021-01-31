package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Page
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.AnalysisResultIndex
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq

final class PageOps[F[_]] private (page: Page)(implicit index: AnalysisResultIndex, F: SyntaxErrorContext[F])
    extends BlockCommonOps[F, Page](page) {
  def siblings: F[ArraySeq[Page]] = F.lift(index.siblingsOf(page))
}

object PageOps {
  trait ToPageOps {
    implicit def toPageOps(page: Page)(implicit index: AnalysisResultIndex): PageOps[ExceptionOr] =
      new PageOps(page)
  }

  trait ToUnsafePageOps {
    implicit def toUnsafePageOps(page: Page)(implicit index: AnalysisResultIndex): PageOps[SyntaxErrorContext.Unsafe] =
      new PageOps(page)
  }
}
