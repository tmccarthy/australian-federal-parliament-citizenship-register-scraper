package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{Line, Page}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.{AnalysisResultIndex, SiblingsUnderPage}
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq

final class LineOps[F[_]] private (line: Line)(implicit index: AnalysisResultIndex, F: SyntaxErrorContext[F])
    extends BlockCommonOps[F, Line](line) {
  def parent: F[Page] =
    F.lift(index.parentOf(line))

  def siblings: F[ArraySeq[SiblingsUnderPage]] =
    F.lift(index.siblingsOf(line))
}

object LineOps {
  trait ToLineOps {
    implicit def toLineOps(line: Line)(implicit index: AnalysisResultIndex): LineOps[ExceptionOr] =
      new LineOps(line)
  }

  trait ToUnsafeLineOps {
    implicit def toUnsafeLineOps(line: Line)(implicit index: AnalysisResultIndex): LineOps[SyntaxErrorContext.Unsafe] =
      new LineOps(line)
  }
}
