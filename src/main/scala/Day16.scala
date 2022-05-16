import scala.collection.mutable

object Day16 {

  class BitStrBuffer(val binStr: String) {

    private val buffer = new mutable.StringBuilder(binStr)
    private var index = 0

    def read(n: Int): BitStrBuffer = {
      val v = buffer.substring(index, index + n)
      index += n
      BitStrBuffer(v)
    }

    def value: BigInt = BigInt(buffer.substring(index), 2)

    override def toString: String = s"$buffer @ $index"

    def isBitSet(n: Int): Boolean = {
      if (buffer.length <= n) return false
      buffer.charAt(buffer.length - n - 1) == '1'
    }

    def isHeadBitSet: Boolean = {
      buffer.charAt(index) == '1'
    }

    def dropHeadBit: BitStrBuffer = {
      BitStrBuffer(buffer.substring(1))
    }

    def slice(length: Int) = {
      val subBuffer = buffer.substring(index, index + length)
      index += length
      BitStrBuffer(subBuffer)
    }

    def nonEmpty: Boolean = index < buffer.length

    def append(other: BitStrBuffer) = {
      buffer.append(other.buffer)
    }
  }

  object BitStrBuffer {
    def fromHex(hex: String) = new BitStrBuffer(BigInt(hex, 16).toString(2).reverse.padTo(hex.length * 4, '0').reverse)

    def apply() = new BitStrBuffer("")

    def apply(binStr: String) = new BitStrBuffer(binStr)
  }

  object PacketType extends Enumeration {
    type PacketType = Value
    val LiteralValue = Value
    val Operator = Value
  }

  sealed abstract class Packet(val version: Int, val typeID: Int) {
    def versionChecksum: Int = this match {
      case Operator(version, _, packets) => version + packets.map(_.versionChecksum).sum
      case LiteralValue(version, _, _) => version
    }

    def value: Long
  }

  object Packet {
    def read(buffer: BitStrBuffer): Packet = {
      val version = buffer.read(3).value.toInt
      val typeID = buffer.read(3).value.toInt

      if (typeID == 4) {
        LiteralValue.read(version, typeID, buffer)
      } else {
        Operator.read(version, typeID, buffer)
      }
    }
  }

  case class LiteralValue(override val version: Int, override val typeID: Int, override val value: Long) extends Packet(version, typeID) {
    val packetType: PacketType.PacketType = PacketType.LiteralValue
  }

  object LiteralValue {
    def read(version: Int, typeID: Int, buffer: BitStrBuffer): LiteralValue = {
      val valueBuffer = BitStrBuffer()
      var continue: Boolean = true
      do {
        continue = buffer.isHeadBitSet
        valueBuffer.append(buffer.read(5).dropHeadBit)
      } while (continue)
      LiteralValue(version, typeID, valueBuffer.value.toLong)
    }
  }

  object OperatorType extends Enumeration {
    type OperatorType = Value
    val Sum = Value(0)
    val Product = Value(1)
    val Minimum = Value(2)
    val Maximum = Value(3)
    val GreaterThan = Value(5)
    val LessThan = Value(6)
    val EqualTo = Value(7)
  }

  case class Operator(
      override val version: Int,
      override val typeID: Int,
      packets: List[Packet],
  ) extends Packet(version, typeID) {
    val packetType: PacketType.PacketType = PacketType.Operator
    val operatorType: OperatorType.OperatorType = OperatorType(typeID)

    override def value: Long = operatorType match {
      case OperatorType.Sum => packets.map(_.value).sum
      case OperatorType.Product => packets.foldLeft(1L)(_ * _.value)
      case OperatorType.Minimum => (packets minBy { _.value }).value
      case OperatorType.Maximum => (packets maxBy { _.value }).value
      case OperatorType.GreaterThan => if (packets(1).value < packets(0).value) 1 else 0
      case OperatorType.LessThan => if (packets(0).value < packets(1).value) 1 else 0
      case OperatorType.EqualTo => if (packets(0).value == packets(1).value) 1 else 0
    }
  }

  object Operator {
    def read(version: Int, typeID: Int, buffer: BitStrBuffer): Operator = {
      val indicator = buffer.read(1).toString.charAt(0)
      if (indicator == '0') {
        readBitsLengthOperator(version, typeID, buffer)
      } else {
        readPacketCountOperator(version, typeID, buffer)
      }
    }

    private def readBitsLengthOperator(version: Int, typeID: Int, buffer: BitStrBuffer): Operator = {
      val bitsLength = buffer.read(15).value.toInt
      val subPacketsBuffer = buffer.slice(bitsLength)
      var packets: List[Packet] = Nil
      while (subPacketsBuffer.nonEmpty) {
        packets = packets :+ Packet.read(subPacketsBuffer)
      }
      Operator(version, typeID, packets)
    }

    private def readPacketCountOperator(version: Int, typeID: Int, buffer: BitStrBuffer): Operator = {
      val packetCount = buffer.read(11).value.toInt
      var packets: List[Packet] = Nil
      for (_ <- 0 until packetCount) {
        packets = packets :+ Packet.read(buffer)
      }
      Operator(version, typeID, packets)
    }
  }
}
