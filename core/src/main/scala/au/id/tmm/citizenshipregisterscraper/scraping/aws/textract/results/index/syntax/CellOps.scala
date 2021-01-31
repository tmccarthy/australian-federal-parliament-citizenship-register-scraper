package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Table
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.AnalysisResultIndex
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq

final class CellOps[F[_]] private (cell: Table.Cell)(implicit index: AnalysisResultIndex, F: SyntaxErrorContext[F])
    extends BlockCommonOps[F, Table.Cell](cell) {
  def parent: F[Table] =
    F.lift(index.parentOf(cell))

  def siblings: F[ArraySeq[Table.Cell]] =
    F.lift(index.siblingsOf(cell))
}

object CellOps {
  trait ToCellOps {
    implicit def toCellOps(cell: Table.Cell)(implicit index: AnalysisResultIndex): CellOps[ExceptionOr] =
      new CellOps(cell)
  }

  trait ToUnsafeCellOps {
    implicit def toUnsafeCellOps(
      cell: Table.Cell,
    )(implicit
      index: AnalysisResultIndex,
    ): CellOps[SyntaxErrorContext.Unsafe] =
      new CellOps(cell)
  }
}
