package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

final case class SelectionElement(
  id: BlockId,
  page: PageNumber,
  geometry: Geometry,
  status: SelectionElement.Status,
)

object SelectionElement {
  sealed abstract class Status(val isSelected: Boolean)

  object Status {
    case object Selected extends Status(isSelected = true)
    case object NotSelected extends Status(isSelected = false)
  }
}