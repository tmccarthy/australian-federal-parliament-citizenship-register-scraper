package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.{Page, Table}
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.{AnalysisResultIndex, SiblingsUnderPage}
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq

final class TableOps[F[_]] private (table: Table)(implicit index: AnalysisResultIndex, F: SyntaxErrorContext[F])
    extends BlockCommonOps[F, Table](table) {
  def parent: F[Page] =
    F.lift(index.parentOf(table))

  def siblings: F[ArraySeq[SiblingsUnderPage]] =
    F.lift(index.siblingsOf(table))

  def findCell(columnIndex: Int, rowIndex: Int): F[Table.Cell] =
    F.lift(index.findCell(table, columnIndex, rowIndex))
}

object TableOps {
  trait ToTableOps {
    implicit def toTableOps(table: Table)(implicit index: AnalysisResultIndex): TableOps[ExceptionOr] =
      new TableOps(table)
  }

  trait ToUnsafeTableOps {
    implicit def toUnsafeTableOps(
      table: Table,
    )(implicit
      index: AnalysisResultIndex,
    ): TableOps[SyntaxErrorContext.Unsafe] =
      new TableOps(table)
  }
}
