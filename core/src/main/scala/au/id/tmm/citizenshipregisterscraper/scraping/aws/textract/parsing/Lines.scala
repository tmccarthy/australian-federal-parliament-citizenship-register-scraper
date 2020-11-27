package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.parsing

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.utilities.errors.ExceptionOr
import software.amazon.awssdk.services.textract.{model => sdk}

import scala.collection.MapView

private[parsing] object Lines {

  import Common._
  import Relationships._

  def parseLine(
    wordLookup: Map[BlockId, Word],
    selectionElementLookup: Map[BlockId, SelectionElement],
    block: sdk.Block,
  ): ExceptionOr[Line] =
    for {
      _          <- requireBlockType(block, sdk.BlockType.LINE)
      id         <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry   <- parseGeometry(block.geometry)
      text       <- requireNonNull(block.text)
      childLookup: Map[BlockId, Line.Child] = (
          (wordLookup.view.mapValues(Line.Child.OfWord): MapView[BlockId, Line.Child]) ++
            (selectionElementLookup.view.mapValues(Line.Child.OfSelectionElement): MapView[BlockId, Line.Child])
      ).toMap
      children <- lookupOrFail(childLookup, block, sdk.RelationshipType.CHILD)
    } yield Line(
      id,
      pageNumber,
      geometry,
      text,
      children,
    )

}
