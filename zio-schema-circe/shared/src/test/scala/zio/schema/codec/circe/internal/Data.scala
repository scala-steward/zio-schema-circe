package zio.schema.codec.circe.internal

import io.circe._
import io.circe.generic.semiauto.deriveEncoder
import zio.prelude.NonEmptySet
import zio.schema.CaseSet._
import zio.schema._
import zio.schema.annotation._
import zio.test._
import zio.{Chunk, NonEmptyChunk}

import scala.collection.immutable.ListMap

protected[circe] object Data {

  def genNonEmptyChunkOf[R, A](g: Gen[R, A]): Gen[R, NonEmptyChunk[A]] = {
    (Gen.chunkOf1(g) <*> Gen.chunkOf(g)).map { case (nonEmptyChunk, chunk) =>
      nonEmptyChunk ++ chunk
    }
  }

  case class Key(name: String, index: Int)

  val genKey: Gen[Sized, Key] = {
    for {
      name  <- Gen.string
      index <- Gen.int
    } yield Key(name, index)
  }

  object Key {
    implicit lazy val schema: Schema[Key] = DeriveSchema.gen[Key]
  }

  case class Value(first: Int, second: Boolean)

  object Value {

    implicit lazy val schema: Schema[Value] = DeriveSchema.gen[Value]
  }

  val genValue: Gen[Sized, Value] =
    for {
      first  <- Gen.int
      second <- Gen.boolean
    } yield Value(first, second)

  def genNonEmptySetOf[R, A](g: Gen[R, A]): Gen[R, NonEmptySet[A]] =
    (g <*> Gen.setOf(g)).map { case (first, set) => NonEmptySet.fromSet(first, set) }

  case class SearchRequest(query: String, size: Int, page: Int, nextPage: Option[String])

  object SearchRequest {
    implicit val encoder: Encoder[SearchRequest] = deriveEncoder
  }

  case class OptionalSearchRequest(
    query: String,
    size: Int,
    page: Int,
    @optionalField nextPage: String,
  )

  object OptionalSearchRequest {
    implicit val encoder: Encoder[OptionalSearchRequest] = deriveEncoder
  }

  final case class Person(name: String, age: Int)

  object Person {

    implicit val encoder: Encoder[Person] = deriveEncoder

    val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  @rejectExtraFields
  final case class PersonWithRejectExtraFields(name: String, age: Int)

  object PersonWithRejectExtraFields {

    implicit val encoder: Encoder[PersonWithRejectExtraFields] = deriveEncoder

    val schema: Schema[PersonWithRejectExtraFields] = DeriveSchema.gen[PersonWithRejectExtraFields]
  }

  case class FieldDefaultValueSearchRequest(
    query: String,
    page: Int,
    size: Int,
    @fieldDefaultValue("test") nextPage: String,
  )

  object FieldDefaultValueSearchRequest {

    implicit val encoder: Encoder[FieldDefaultValueSearchRequest] = deriveEncoder
  }

  val searchRequestGen: Gen[Sized, SearchRequest] =
    for {
      query    <- Gen.string
      page     <- Gen.int(Int.MinValue, Int.MaxValue)
      size     <- Gen.int(Int.MinValue, Int.MaxValue)
      nextPage <- Gen.option(Gen.asciiString)
    } yield SearchRequest(query, page, size, nextPage)

  val optionalSearchRequestGen: Gen[Sized, OptionalSearchRequest] =
    for {
      query    <- Gen.string
      page     <- Gen.int(Int.MinValue, Int.MaxValue)
      size     <- Gen.int(Int.MinValue, Int.MaxValue)
      nextPage <- Gen.asciiString
    } yield OptionalSearchRequest(query, page, size, nextPage)

  val searchRequestSchema: Schema[SearchRequest] = DeriveSchema.gen[SearchRequest]

  val optionalSearchRequestSchema: Schema[OptionalSearchRequest] = DeriveSchema.gen[OptionalSearchRequest]

  case class SearchRequestWithTransientField(
    query: String,
    page: Int,
    size: Int,
    @transientField nextPage: String = "",
  )

  val searchRequestWithTransientFieldSchema: Schema[SearchRequestWithTransientField] =
    DeriveSchema.gen[SearchRequestWithTransientField]

  val fieldDefaultValueSearchRequestSchema: Schema[FieldDefaultValueSearchRequest] =
    DeriveSchema.gen[FieldDefaultValueSearchRequest]

  val recordSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "foo",
      Schema.Primitive(StandardType.StringType),
      get0 = (p: ListMap[String, _]) => p("foo").asInstanceOf[String],
      set0 = (p: ListMap[String, _], v: String) => p.updated("foo", v),
    ),
    Schema
      .Field(
        "bar",
        Schema.Primitive(StandardType.IntType),
        annotations0 = Chunk(fieldDefaultValue(1)),
        get0 = (p: ListMap[String, _]) => p("bar").asInstanceOf[Int],
        set0 = (p: ListMap[String, _], v: Int) => p.updated("bar", v),
      ),
  )

  val recordWithOptionSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "foo",
      Schema.Primitive(StandardType.StringType).optional,
      get0 = (p: ListMap[String, _]) => p("foo").asInstanceOf[Option[String]],
      set0 = (p: ListMap[String, _], v: Option[String]) => p.updated("foo", v),
    ),
    Schema
      .Field(
        "bar",
        Schema.Primitive(StandardType.IntType).optional,
        get0 = (p: ListMap[String, _]) => p("bar").asInstanceOf[Option[Int]],
        set0 = (p: ListMap[String, _], v: Option[Int]) => p.updated("bar", v),
      ),
  )

  val nestedRecordSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "l1",
      Schema.Primitive(StandardType.StringType),
      get0 = (p: ListMap[String, _]) => p("l1").asInstanceOf[String],
      set0 = (p: ListMap[String, _], v: String) => p.updated("l1", v),
    ),
    Schema.Field(
      "l2",
      recordSchema,
      get0 = (p: ListMap[String, _]) => p("l2").asInstanceOf[ListMap[String, _]],
      set0 = (p: ListMap[String, _], v: ListMap[String, _]) => p.updated("l2", v),
    ),
  )

  val enumSchema: Schema[Any] = Schema.enumeration[Any, CaseSet.Aux[Any]](
    TypeId.Structural,
    caseOf[String, Any]("string")(_.asInstanceOf[String])(_.asInstanceOf[Any])(_.isInstanceOf[String]) ++ caseOf[
      Int,
      Any,
    ]("int")(_.asInstanceOf[Int])(_.asInstanceOf[Any])(_.isInstanceOf[Int]) ++ caseOf[
      Boolean,
      Any,
    ]("boolean")(_.asInstanceOf[Boolean])(_.asInstanceOf[Any])(_.isInstanceOf[Boolean]),
  )

  sealed trait OneOf
  case class StringValue(value: String)   extends OneOf
  case class IntValue(value: Int)         extends OneOf
  case class BooleanValue(value: Boolean) extends OneOf

  object OneOf {
    implicit val schema: Schema[OneOf] = DeriveSchema.gen[OneOf]
  }

  case class Enumeration(oneOf: OneOf)

  object Enumeration {
    implicit val schema: Schema[Enumeration] = DeriveSchema.gen[Enumeration]
  }

  @discriminatorName("_type")
  sealed trait OneOf2
  case class StringValue2(value: String)                       extends OneOf2
  case class IntValue2(value: Int)                             extends OneOf2
  case class BooleanValue2(value: Boolean)                     extends OneOf2
  case class StringValue2Multi(value1: String, value2: String) extends OneOf2

  case class Enumeration2(oneOf: OneOf2)

  object Enumeration2 {
    implicit val schema: Schema[Enumeration2] = DeriveSchema.gen[Enumeration2]
  }

  @noDiscriminator
  sealed trait OneOf3
  case class StringValue3(value: String)                       extends OneOf3
  case class IntValue3(value: Int)                             extends OneOf3
  case class BooleanValue3(value: Boolean)                     extends OneOf3
  case class StringValue3Multi(value1: String, value2: String) extends OneOf3
  case class Nested(oneOf: OneOf3)                             extends OneOf3

  case class Enumeration3(oneOf: OneOf3)

  object Enumeration3 {
    implicit val schema: Schema[Enumeration3] = DeriveSchema.gen[Enumeration3]
  }

  sealed trait Color

  object Color {
    case object Red extends Color

    @caseName("Green")
    case object Grass extends Color

    @caseNameAliases("LightBlue", "DarkBlue")
    case object Blue extends Color

    implicit val schema: Schema[Color] = DeriveSchema.gen[Color]
  }

  @annotation.discriminatorName("type")
  sealed trait Command

  object Command {
    case class Buy(credits: Int) extends Command
    case object Cash             extends Command

    implicit val schema: Schema[Command] = DeriveSchema.gen[Command]
  }

  case object Singleton
  implicit val schemaObject: Schema[Singleton.type] = DeriveSchema.gen[Singleton.type]

  sealed trait PaymentMethod

  object PaymentMethod {

    @caseNameAliases("credit_card", "cc") final case class CreditCard(
      number: String,
      expirationMonth: Int,
      expirationYear: Int,
    ) extends PaymentMethod

    @caseName("wire_transfer") final case class WireTransfer(accountNumber: String, bankCode: String)
        extends PaymentMethod

    @transientCase final case class PayPal(email: String) extends PaymentMethod

    implicit lazy val schema: Schema[PaymentMethod] = DeriveSchema.gen[PaymentMethod]
  }

  @discriminatorName("type") sealed trait Subscription

  object Subscription {

    @caseName("recurring") final case class Recurring(
      period: String,
      amount: Int,
    ) extends Subscription

    @caseNameAliases("one_time", "onetime") final case class OneTime(
      amount: Int,
    ) extends Subscription

    @caseName("unlimited") final case class Unlimited(until: Option[Long]) extends Subscription

    implicit lazy val schema: Schema[Subscription] = DeriveSchema.gen[Subscription]
  }

  case class Order(@fieldNameAliases("order_id", "id") orderId: Int, value: BigDecimal, description: String)

  object Order {
    implicit lazy val schema: Schema[Order] = DeriveSchema.gen[Order]
  }
  @noDiscriminator sealed trait Prompt

  object Prompt {
    final case class Single(value: String)         extends Prompt
    final case class Multiple(value: List[String]) extends Prompt

    implicit lazy val schema: Schema[Prompt] = DeriveSchema.gen[Prompt]
  }

  final case class WithOptionFields(a: Option[String], b: Option[Int])

  object WithOptionFields {
    implicit lazy val schema: Schema[WithOptionFields] = DeriveSchema.gen[WithOptionFields]
  }

  final case class WithComplexOptionField(order: Option[Order])

  object WithComplexOptionField {
    implicit lazy val schema: Schema[WithComplexOptionField] = DeriveSchema.gen[WithComplexOptionField]
  }

  final case class WithOptField(@optionalField list: List[String], @optionalField map: Map[String, Int])

  object WithOptField {
    implicit lazy val schema: Schema[WithOptField] = DeriveSchema.gen[WithOptField]
  }

  final case class ListAndMapAndOption(list: List[String], map: Map[String, Int], option: Option[String])

  object ListAndMapAndOption {
    implicit lazy val schema: Schema[ListAndMapAndOption] = DeriveSchema.gen[ListAndMapAndOption]
  }

  final case class SetWrapper(set: Set[String])

  object SetWrapper {
    implicit lazy val schema: Schema[SetWrapper] = DeriveSchema.gen[SetWrapper]
  }

  final case class VectorWrapper(sequence: Vector[String])

  object VectorWrapper {
    implicit lazy val schema: Schema[VectorWrapper] = DeriveSchema.gen[VectorWrapper]
  }

  final case class ChunkWrapper(chunk: Chunk[String])

  object ChunkWrapper {
    implicit lazy val schema: Schema[ChunkWrapper] = DeriveSchema.gen[ChunkWrapper]
  }

  final case class KeyWrapper(key: String)

  object KeyWrapper {
    implicit lazy val schema: Schema[KeyWrapper] = Schema[String].transform(KeyWrapper.apply, _.key)
  }

  final case class ValueWrapper(value: String)

  object ValueWrapper {
    implicit lazy val schema: Schema[ValueWrapper] = DeriveSchema.gen[ValueWrapper]
  }

  final case class MapOfComplexKeysAndValues(map: Map[KeyWrapper, ValueWrapper])

  object MapOfComplexKeysAndValues {
    implicit lazy val mapSchema: Schema[Map[KeyWrapper, ValueWrapper]] = Schema.map[KeyWrapper, ValueWrapper]
    implicit lazy val schema: Schema[MapOfComplexKeysAndValues]        = DeriveSchema.gen[MapOfComplexKeysAndValues]
  }

  final case class AllOptionalFields(
    name: Option[String],
    mode: Option[Int],
    active: Option[Boolean],
  )

  object AllOptionalFields {
    implicit lazy val schema: Schema[AllOptionalFields] = DeriveSchema.gen[AllOptionalFields]
  }

  case class RecordExample(
    @fieldName("$f1") f1: Option[String], // the only field that does not have a default value
    @fieldNameAliases("field2") f2: Option[String] = None,
    @transientField f3: Option[String] = None,
    f4: Option[String] = None,
    f5: Option[String] = None,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: Option[String] = None,
    f18: Option[String] = None,
    f19: Option[String] = None,
    f20: Option[String] = None,
    f21: Option[String] = None,
    f22: Option[String] = None,
    @fieldName("$f23") f23: Option[String] = None,
  )

  object RecordExample {

    implicit lazy val schema: Schema[RecordExample] = DeriveSchema.gen[RecordExample]
  }

  case class RecordExampleWithOptField(
    @fieldName("$f1") f1: Option[String], // the only field that does not have a default value
    @optionalField @fieldNameAliases("field2") f2: Option[String] = None,
    @transientField f3: Option[String] = None,
    @optionalField f4: String,
    @optionalField @fieldDefaultValue("hello") f5: String,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: Option[String] = None,
    f18: Option[String] = None,
    f19: Option[String] = None,
    f20: Option[String] = None,
    f21: Option[String] = None,
    f22: Option[String] = None,
    @fieldName("$f23") f23: Option[String] = None,
  )

  object RecordExampleWithOptField {

    implicit lazy val schema: Schema[RecordExampleWithOptField] =
      DeriveSchema.gen[RecordExampleWithOptField]
  }

  sealed trait Enum23Cases

  object Enum23Cases {

    @caseName("NumberOne")
    @caseNameAliases("One")
    case class Case1(value: String) extends Enum23Cases

    case class Case2(value: Int) extends Enum23Cases

    case class Case3(value: String) extends Enum23Cases

    case class Case4(value: String) extends Enum23Cases

    case class Case5(value: String) extends Enum23Cases

    case class Case6(value: String) extends Enum23Cases

    case class Case7(value: String) extends Enum23Cases

    case class Case8(value: String) extends Enum23Cases

    case class Case9(value: String) extends Enum23Cases

    case class Case10(value: String) extends Enum23Cases

    case class Case11(value: String) extends Enum23Cases

    case class Case12(value: String) extends Enum23Cases

    case class Case13(value: String) extends Enum23Cases

    case class Case14(value: String) extends Enum23Cases

    case class Case15(value: String) extends Enum23Cases

    case class Case16(value: String) extends Enum23Cases

    case class Case17(value: String) extends Enum23Cases

    case class Case18(value: String) extends Enum23Cases

    case class Case19(value: String) extends Enum23Cases

    case class Case20(value: String) extends Enum23Cases

    case class Case21(value: String) extends Enum23Cases

    case class Case22(value: String) extends Enum23Cases

    case class Case23(value: String) extends Enum23Cases

    implicit lazy val schema: Schema[Enum23Cases] = DeriveSchema.gen[Enum23Cases]
  }
}
