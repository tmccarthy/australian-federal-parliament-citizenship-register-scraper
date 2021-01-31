package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{KeyValueSet, Page}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.AnalysisResultIndex
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq

final class KeyValueSetOps[F[_]] private (
  keyValueSet: KeyValueSet,
)(implicit
  index: AnalysisResultIndex,
  F: SyntaxErrorContext[F],
) {
  def parent: F[Page] = F.lift(index.parentOf(keyValueSet))
  def siblings: F[ArraySeq[KeyValueSet]] = F.lift(index.siblingsOf(keyValueSet))
}

object KeyValueSetOps {
  trait ToKeyValueSetOps {
    implicit def toKeyValueSetOps(
      keyValueSet: KeyValueSet,
    )(implicit
      index: AnalysisResultIndex,
    ): KeyValueSetOps[ExceptionOr] =
      new KeyValueSetOps(keyValueSet)
  }

  trait ToUnsafeKeyValueSetOps {
    implicit def toUnsafeKeyValueSetOps(
      keyValueSet: KeyValueSet,
    )(implicit
      index: AnalysisResultIndex,
    ): KeyValueSetOps[SyntaxErrorContext.Unsafe] =
      new KeyValueSetOps(keyValueSet)
  }
}
