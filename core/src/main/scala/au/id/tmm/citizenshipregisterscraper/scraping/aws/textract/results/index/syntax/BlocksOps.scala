package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Block
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.{AnalysisResultIndex, Searches}
import au.id.tmm.utilities.errors.ExceptionOr

final class BlocksOps[F[_]] private (
  blocks: Seq[Block],
)(implicit
  index: AnalysisResultIndex,
  F: SyntaxErrorContext[F],
) {
  def recursivelySearch[B2 <: Block](collect: PartialFunction[Block, B2]): F[LazyList[B2]] =
    F.lift(Searches.recursivelySearch(blocks)(collect))
}

object BlocksOps {
  trait ToBlocksOps {
    implicit def toBlocksOps(blocks: Seq[Block])(implicit index: AnalysisResultIndex): BlocksOps[ExceptionOr] =
      new BlocksOps(blocks)
  }

  trait ToUnsafeBlocksOps {
    implicit def toUnsafeBlocksOps(
      blocks: Seq[Block],
    )(implicit
      index: AnalysisResultIndex,
    ): BlocksOps[SyntaxErrorContext.Unsafe] =
      new BlocksOps(blocks)
  }
}
