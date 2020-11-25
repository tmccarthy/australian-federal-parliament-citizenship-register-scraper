package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.parsing

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model._
import au.id.tmm.utilities.errors.ExceptionOr
import software.amazon.awssdk.services.textract.{model => sdk}

import scala.collection.MapView

private[parsing] object Pages {

  import Common._
  import Relationships._

  def parsePage(
    lineLookup: Map[BlockId, Line],
    tableLookup: Map[BlockId, Table],
    keyValueSetLookup: Map[BlockId, KeyValueSet],
    block: sdk.Block,
  ): ExceptionOr[Page] =
    for {
      _          <- requireBlockType(block, sdk.BlockType.PAGE)
      id         <- BlockId.fromString(block.id)
      pageNumber <- PageNumber(block.page)
      geometry   <- parseGeometry(block.geometry)

      childLookup: Map[BlockId, Page.Child] = (
        (lineLookup.view.mapValues(Page.Child.OfLine): MapView[BlockId, Page.Child]) ++
          (tableLookup.view.mapValues(Page.Child.OfTable): MapView[BlockId, Page.Child])
        ).toMap

      // TODO support key value sets
      children <- lookupOrIgnore(childLookup, block.relationships, sdk.RelationshipType.CHILD)
    } yield Page(
      id,
      pageNumber,
      geometry,
      children,
    )

}
