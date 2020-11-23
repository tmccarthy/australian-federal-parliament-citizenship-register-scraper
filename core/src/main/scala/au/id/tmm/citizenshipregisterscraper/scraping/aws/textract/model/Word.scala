package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.model

final case class Word(
  id: BlockId,
  page: PageNumber,
  geometry: Geometry,
  text: String,
  confidence: Confidence,
  textType: Word.TextType,
)

object Word {
  sealed trait TextType

  object TextType {
    case object Printed extends TextType
    case object Handwriting extends TextType
  }
}
