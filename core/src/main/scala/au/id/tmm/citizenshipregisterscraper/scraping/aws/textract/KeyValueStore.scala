package au.id.tmm.citizenshipregisterscraper.scraping.aws.textract

import java.util.UUID

import au.id.tmm.digest4s.digest.SHA512Digest
import au.id.tmm.utilities.errors.ExceptionOr
import cats.effect.IO

import scala.collection.immutable.ArraySeq
import java.nio.ByteBuffer

trait KeyValueStore {

  def put[K : KeyValueStore.Codec, V : KeyValueStore.Codec](k: K, v: V): IO[Unit]

  def get[K : KeyValueStore.Codec, V : KeyValueStore.Codec](k: K): IO[Option[V]]

}

object KeyValueStore {
  trait Codec[A] {
    def encode(a: A): ArraySeq[Byte]
    def decode(bytes: ArraySeq.ofByte): ExceptionOr[A]
  }

  object Codec {
    implicit val forDigest: Codec[SHA512Digest] = new Codec[SHA512Digest] {
      override def encode(a: SHA512Digest): ArraySeq[Byte] = a.asBytes
      override def decode(bytes: ArraySeq.ofByte): ExceptionOr[SHA512Digest] = Right(SHA512Digest(bytes))
    }
    implicit val forUUID: Codec[TextractJobId] = new Codec[TextractJobId] {
      override def encode(jobId: TextractJobId): ArraySeq[Byte] = {
        val bytes = ByteBuffer.wrap(new Array[Byte](16))
        bytes.putLong(jobId.asUUID.getMostSignificantBits)
        bytes.putLong(jobId.asUUID.getLeastSignificantBits)
        new ArraySeq.ofByte(bytes.array())
      }
      override def decode(bytes: ArraySeq.ofByte): ExceptionOr[TextractJobId] =
        ExceptionOr.catchIn(TextractJobId(UUID.nameUUIDFromBytes(bytes.unsafeArray)))
    }
  }
}
