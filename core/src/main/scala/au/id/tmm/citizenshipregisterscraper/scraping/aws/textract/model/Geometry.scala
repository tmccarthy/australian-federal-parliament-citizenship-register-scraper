package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

import scala.collection.immutable.ArraySeq

final case class Geometry(
  boundingBox: Geometry.BoundingBox,
  polygon: Geometry.Polygon,
)

object Geometry {
  final case class BoundingBox(
    left: Double,
    top: Double,
    height: Double,
    width: Double,
  )

  final case class Polygon(
    points: ArraySeq[Polygon.Point],
  ) extends AnyVal

  object Polygon {
    final case class Point(x: Double, y: Double)
  }
}
