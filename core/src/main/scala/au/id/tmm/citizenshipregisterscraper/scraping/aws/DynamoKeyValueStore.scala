package au.id.tmm.citizenshipregisterscraper.scraping.aws

import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import cats.effect.{IO, Resource}
import cats.syntax.applicativeError._
import cats.syntax.traverse.toTraverseOps
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model._

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._

final class DynamoKeyValueStore private (
  tableName: String,
  client: DynamoDbClient,
) extends KeyValueStore {

  override def put[K : KeyValueStore.Codec, V : KeyValueStore.Codec](k: K, v: V): IO[Unit] =
    for {
      request <- IO.pure(
        PutItemRequest
          .builder()
          .tableName(tableName)
          .item(
            Map(
              "key"   -> asAttribute(k),
              "value" -> asAttribute(v),
            ).asJava,
          )
          .build(),
      )

      response <- IO(client.putItem(request))
    } yield ()

  override def get[K : KeyValueStore.Codec, V : KeyValueStore.Codec](k: K): IO[Option[V]] =
    for {
      request <- IO.pure(
        GetItemRequest
          .builder()
          .tableName(tableName)
          .key(
            Map(
              "key" -> asAttribute(k),
            ).asJava,
          )
          .build(),
      )

      response <- IO(client.getItem(request))

      attributeValue <-
        Option(response.item())
          .filterNot(_.isEmpty)
          .traverse { javaMap =>
            IO.fromEither {
              javaMap.asScala
                .get("key")
                .toRight(GenericException("No key for item"))
            }
          }

      value <- IO.fromEither(attributeValue.traverse(fromAttribute[V](_)))
    } yield value

  private def asAttribute[A : KeyValueStore.Codec](a: A): AttributeValue =
    AttributeValue
      .builder()
      .b(SdkBytes.fromByteArrayUnsafe(KeyValueStore.Codec[A].encode(a).unsafeArray))
      .build()

  private def fromAttribute[A : KeyValueStore.Codec](attributeValue: AttributeValue): ExceptionOr[A] =
    for {
      sdkBytes <- Option(attributeValue.b()).toRight(GenericException("Value wasn't bytes"))
      bytes    = new ArraySeq.ofByte(sdkBytes.asByteArrayUnsafe)
      decoded  <- KeyValueStore.Codec[A].decode(bytes)
    } yield decoded

}

object DynamoKeyValueStore {

  def apply(tableName: String): Resource[IO, DynamoKeyValueStore] =
    for {
      client <- Resource.make(IO(DynamoDbClient.builder().build()))(dynamoDbClient => IO(dynamoDbClient.close()))
      dynamoKeyValueStore <- Resource.liftF(makeTableIfNoneDefined(client, tableName).as(new DynamoKeyValueStore(tableName, client)))
    } yield dynamoKeyValueStore

  private def makeTableIfNoneDefined(client: DynamoDbClient, tableName: String): IO[Unit] =
    for {
      describeTableRequest <- IO.pure(DescribeTableRequest.builder().tableName(tableName).build())
      tableExists <- IO(client.describeTable(describeTableRequest)).as(true).recover {
        case _: ResourceNotFoundException => false
      }

      _ <-
        if (tableExists) {
          IO.unit
        } else {
          for {
            createTableRequest <- IO.pure(
              CreateTableRequest
                .builder()
                .tableName(tableName)
                .billingMode(BillingMode.PAY_PER_REQUEST)
                .attributeDefinitions(AttributeDefinition.builder().attributeName("key").attributeType(ScalarAttributeType.B).build())
                .keySchema(KeySchemaElement.builder().attributeName("key").keyType(KeyType.HASH).build())
                .build()
            )
            createTableResponse <- IO(client.createTable(createTableRequest)) // TODO need to wait for the able creation to complete
          } yield ()
        }

    } yield ()

}
