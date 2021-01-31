package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{KeyValueSet, Page}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.AnalysisResultIndex
import au.id.tmm.utilities.errors.ExceptionOr

final class KeyOps[F[_]] private (key: KeyValueSet.Key)(implicit index: AnalysisResultIndex, F: SyntaxErrorContext[F])
    extends BlockCommonOps[F, KeyValueSet.Key](key) {
  def parent: F[Page] = F.lift(index.parentOf(key))
  def kvSet: F[KeyValueSet] = F.lift(index.kvSetFor(key))
  def value: F[KeyValueSet.Value] = F.lift(index.valueFor(key))
}

object KeyOps {
  trait ToKeyOps {
    implicit def toKeyOps(key: KeyValueSet.Key)(implicit index: AnalysisResultIndex): KeyOps[ExceptionOr] =
      new KeyOps(key)
  }

  trait ToUnsafeKeyOps {
    implicit def toUnsafeKeyOps(
      key: KeyValueSet.Key,
    )(implicit
      index: AnalysisResultIndex,
    ): KeyOps[SyntaxErrorContext.Unsafe] =
      new KeyOps(key)
  }
}
