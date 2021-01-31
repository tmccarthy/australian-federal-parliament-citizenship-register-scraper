package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{KeyValueSet, Page}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.AnalysisResultIndex
import au.id.tmm.utilities.errors.ExceptionOr

final class ValueOps[F[_]] private (
  value: KeyValueSet.Value,
)(implicit
  index: AnalysisResultIndex,
  F: SyntaxErrorContext[F],
) extends BlockCommonOps[F, KeyValueSet.Value](value) {
  def parent: F[Page] = F.lift(index.parentOf(value))
  def kvSet: F[KeyValueSet] = F.lift(index.kvSetFor(value))
  def key: F[KeyValueSet.Key] = F.lift(index.keyFor(value))
}

object ValueOps {
  trait ToValueOps {
    implicit def toValueOps(value: KeyValueSet.Value)(implicit index: AnalysisResultIndex): ValueOps[ExceptionOr] =
      new ValueOps(value)
  }

  trait ToUnsafeValueOps {
    implicit def toValueOps(
      value: KeyValueSet.Value,
    )(implicit
      index: AnalysisResultIndex,
    ): ValueOps[SyntaxErrorContext.Unsafe] =
      new ValueOps(value)
  }
}
