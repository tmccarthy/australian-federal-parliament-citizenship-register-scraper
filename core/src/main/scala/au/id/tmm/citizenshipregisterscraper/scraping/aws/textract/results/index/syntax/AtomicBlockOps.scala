package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.AtomicBlock
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.{AnalysisResultIndex, AtomicBlockParent}
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq

final class AtomicBlockOps[F[_]] private (
  atomicBlock: AtomicBlock,
)(implicit
  index: AnalysisResultIndex,
  F: SyntaxErrorContext[F],
) extends BlockCommonOps[F, AtomicBlock](atomicBlock) {
  def parent: F[AtomicBlockParent] =
    F.lift(index.parentOf(atomicBlock))

  def siblings: F[ArraySeq[AtomicBlock]] =
    F.lift(index.siblingsOf(atomicBlock))
}

object AtomicBlockOps {
  trait ToAtomicBlockOps {
    implicit def toAtomicBlockOps(
      atomicBlock: AtomicBlock,
    )(implicit
      index: AnalysisResultIndex,
    ): AtomicBlockOps[ExceptionOr] =
      new AtomicBlockOps(atomicBlock)
  }

  trait ToUnsafeAtomicBlockOps {
    implicit def toUnsafeAtomicBlockOps(
      atomicBlock: AtomicBlock,
    )(implicit
      index: AnalysisResultIndex,
    ): AtomicBlockOps[SyntaxErrorContext.Unsafe] =
      new AtomicBlockOps(atomicBlock)
  }
}
