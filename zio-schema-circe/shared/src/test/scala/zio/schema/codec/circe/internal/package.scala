package zio.schema.codec.circe

package object internal {

  def stringify(str: String): String = io.circe.Encoder.encodeString(str).noSpaces
}
