package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.syntax

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model.Block
import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results.index.{AnalysisResultIndex, Searches}

import scala.reflect.ClassTag

private[syntax] abstract class BlockCommonOps[F[_], B <: Block : ClassTag](
  block: B,
)(implicit
  index: AnalysisResultIndex,
  F: SyntaxErrorContext[F],
) {
  def recursivelySearch[B2 <: Block](collect: PartialFunction[Block, B2]): F[LazyList[B2]] =
    F.lift(Searches.recursivelySearch[B2](block)(collect))

  def recursivelySearchWithPredicate[B2 <: Block : ClassTag](predicate: B2 => Boolean): F[LazyList[B2]] =
    F.lift {
      Searches.recursivelySearch[B2](block) {
        case b2: B2 if predicate(b2) => b2
      }
    }
}
