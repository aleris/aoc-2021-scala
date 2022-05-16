import Day16.{BitStrBuffer, LiteralValue, Operator, OperatorType, Packet}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source


class Day16Suite extends AnyFunSpec {
  describe("BitBuffer") {
    it("isBitSet") {
      val hex4 = BigInt(4).toString(16)
      BitStrBuffer(hex4).isBitSet(2) should be (true)
      BitStrBuffer(hex4).isBitSet(1) should be (false)
      BitStrBuffer(hex4).isBitSet(0) should be (false)
      BitStrBuffer(hex4).isBitSet(3) should be (false)
      BitStrBuffer(hex4).isBitSet(4) should be (false)
    }

    it("parses from hex") {
      BitStrBuffer.fromHex("38006F45291200").toString should be ("00111000000000000110111101000101001010010001001000000000")
    }
  }

  describe("Day 16 test") {
    it("reads LiteralValue") {
      val value = Packet.read(BitStrBuffer.fromHex("D2FE28"))
      value should be (LiteralValue(6, 4, 2021))
    }

    it("reads BitsLengthOperator") {
      val value = Packet.read(BitStrBuffer.fromHex("38006F45291200"))
      value should be (Operator(1, 6, LiteralValue(6, 4, 10) :: LiteralValue(2, 4, 20) :: Nil))
    }

    it("reads PacketCountOperator") {
      val value = Packet.read(BitStrBuffer.fromHex("EE00D40C823060"))
      value should be (Operator(7, 3, LiteralValue(2, 4, 1) :: LiteralValue(4, 4, 2) :: LiteralValue(1, 4, 3) :: Nil))
    }

    it("reads additional example 1") {
      val value = Packet.read(BitStrBuffer.fromHex("8A004A801A8002F478"))
      value.versionChecksum should be (16)
    }

    it("reads additional example 2") {
      val value = Packet.read(BitStrBuffer.fromHex("620080001611562C8802118E34"))
      value.versionChecksum should be (12)
    }

    it("reads additional example 3") {
      val value = Packet.read(BitStrBuffer.fromHex("C0015000016115A2E0802F182340"))
      value.versionChecksum should be (23)
    }

    it("reads additional example 4") {
      val value = Packet.read(BitStrBuffer.fromHex("A0016C880162017C3686B18A3D4780"))
      value.versionChecksum should be (31)
    }

    it("calculates value example 1") {
      val value = Packet.read(BitStrBuffer.fromHex("C200B40A82"))
      value.value should be (3)
    }

    it("calculates value example 2") {
      val value = Packet.read(BitStrBuffer.fromHex("04005AC33890"))
      value.value should be (54)
    }

    it("calculates value example 3") {
      val value = Packet.read(BitStrBuffer.fromHex("880086C3E88112"))
      value.value should be (7)
    }

    it("calculates value example 4") {
      val value = Packet.read(BitStrBuffer.fromHex("CE00C43D881120"))
      value.value should be (9)
    }

    it("calculates value example 5") {
      val value = Packet.read(BitStrBuffer.fromHex("D8005AC2A8F0"))
      value.value should be (1)
    }

    it("calculates value example 6") {
      val value = Packet.read(BitStrBuffer.fromHex("F600BC2D8F"))
      value.value should be (0)
    }

    it("calculates value example 7") {
      val value = Packet.read(BitStrBuffer.fromHex("9C005AC2F8F0"))
      value.value should be (0)
    }

    it("calculates value example 8") {
      val value = Packet.read(BitStrBuffer.fromHex("9C0141080250320F1802104A08"))
      value.value should be (1)
    }
  }


  describe("Day 16") {
    it("a. version numbers of all packets") {
      val hex = Source.fromResource("day_16.txt").getLines().next()
      println(Packet.read(BitStrBuffer.fromHex(hex)).versionChecksum)
    }

    it("b. calculates value") {
      val hex = Source.fromResource("day_16.txt").getLines().next()
      println(Packet.read(BitStrBuffer.fromHex(hex)).value)
    }

  }
}
