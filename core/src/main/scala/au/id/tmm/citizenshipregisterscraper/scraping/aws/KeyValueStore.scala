package au.id.tmm.citizenshipregisterscraper.scraping.aws

import java.nio.charset.{Charset, StandardCharsets}

import au.id.tmm.citizenshipregisterscraper.scraping.aws.textract.TextractJobId
import au.id.tmm.digest4s.digest.SHA512Digest
import au.id.tmm.utilities.errors.ExceptionOr
import cats.Invariant
import cats.effect.IO
import cats.syntax.invariant._

import scala.collection.immutable.ArraySeq

trait KeyValueStore {

  def put[K : KeyValueStore.Codec, V : KeyValueStore.Codec](k: K, v: V): IO[Unit]

  def get[K : KeyValueStore.Codec, V : KeyValueStore.Codec](k: K): IO[Option[V]]

}

object KeyValueStore {
  trait Codec[A] {
    def encode(a: A): ArraySeq.ofByte
    def decode(bytes: ArraySeq.ofByte): ExceptionOr[A]
  }

  object Codec {

    def apply[A : Codec]: Codec[A] = implicitly

    implicit val invariantForCodec: Invariant[Codec] = new Invariant[Codec] {
      override def imap[A, B](fa: Codec[A])(f: A => B)(g: B => A): Codec[B] = new Codec[B] {
        override def encode(b: B): ArraySeq.ofByte = fa.encode(g(b))
        override def decode(bytes: ArraySeq.ofByte): ExceptionOr[B] = fa.decode(bytes).map(f)
      }
    }

    implicit val forString: Codec[String] = new Codec[String] {
      private val charset: Charset = StandardCharsets.UTF_8

      override def encode(a: String): ArraySeq.ofByte = new ArraySeq.ofByte(a.getBytes(charset))

      override def decode(bytes: ArraySeq.ofByte): ExceptionOr[String] = ExceptionOr.catchIn(new String(bytes.unsafeArray, charset))
    }

    implicit val forDigest: Codec[SHA512Digest] = new Codec[SHA512Digest] {
      override def encode(a: SHA512Digest): ArraySeq.ofByte = a.asBytes
      override def decode(bytes: ArraySeq.ofByte): ExceptionOr[SHA512Digest] = Right(SHA512Digest(bytes))
    }

    implicit val forUUID: Codec[TextractJobId] = Codec[String].imap(TextractJobId.apply)(_.asString)
  }
}
