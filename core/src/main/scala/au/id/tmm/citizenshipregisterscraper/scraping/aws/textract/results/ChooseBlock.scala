package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.results

import au.id.tmm.collections.syntax.toIterableOps
import au.id.tmm.utilities.errors.ExceptionOr

import scala.collection.immutable.ArraySeq

trait ChooseBlock[B] {
  def chooseFrom(blocks: ArraySeq[B]): ExceptionOr[B]
}

object ChooseBlock {
  def only[B]: ChooseBlock[B] = blocks => blocks.onlyElementOrException
  def onlyMatching[B, B2 >: B](predicate: B2 => Boolean): ChooseBlock[B] = blocks => blocks.filter(predicate).onlyElementOrException
  def firstBy[B, B2 >: B](ordering: Ordering[B2]): ChooseBlock[B] = blocks => blocks.sorted(ordering).headOrException
}