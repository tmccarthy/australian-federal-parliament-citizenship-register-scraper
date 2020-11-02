package au.id.tmm.citizenshipregisterscraper.documents.senate

import java.net.URI
import java.time.LocalDate

import au.id.tmm.ausgeo.State

final case class DocumentReference(
  state: State,
  senatorName: String,
  lastUpdated: LocalDate,
  documentLocation: URI,
)
