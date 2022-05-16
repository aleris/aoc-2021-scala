import Day18.Parser.{N, P, Resolver}
import Day18.{Number, Pair, Parser}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, equal}
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.io.Source


class Day18Suite extends AnyFunSpec {
  describe("Day 18 test") {

    describe("parses examples") {
      it("parses example 1") {
        val e = Parser.parse("[1,2]")
        e should be(P(N(1), N(2)))
      }

      it("parses example 2") {
        val e = Parser.parse("[[1,2],3]")
        e should be(P(P(N(1), N(2)), N(3)))
      }

      it("parses example 3") {
        val e = Parser.parse("[9,[8,7]]")
        e should be(P(N(9), P(N(8), N(7))))
      }

      it("parses example 4") {
        val e = Parser.parse("[[1,9],[8,5]]")
        e should be(P(P(N(1), N(9)), P(N(8), N(5))))
      }

      it("parses example 5") {
        val e = Parser.parse("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]")
        e should be(
          P(
            P(
              P(P(N(1), N(2)), P(N(3), N(4))),
              P(P(N(5), N(6)), P(N(7), N(8))),
            ),
            N(9),
          )
        )
      }

      it("parses example x") {
        val e = Parser.parse("[[[[1,2],[3,4]],[5,[6,7]]],8]")
        e should be(
          P(
            P(
              P(P(N(1), N(2)), P(N(3), N(4))),
              P(N(5), P(N(6), N(7))),
            ),
            N(8),
          )
        )
      }

      it("parses other examples") {
        """
          |[1,2]
          |[[1,2],3]
          |[9,[8,7]]
          |[[1,9],[8,5]]
          |[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
          |[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
          |[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
          |""".stripMargin.trim.lines().forEach(line => Parser.parse(line))
      }
    }

    it("adds pairs") {
      val e1 = Parser.parse("[1,2]")
      val e2 = Parser.parse("[[3,4],5]")
      val r = e1.asTreeElement() addSimple e2.asTreeElement()
      r.toString should be ("[[1,2],[[3,4],5]]")
    }

    describe("finds right") {
      val root = Parser.parse("[[[3,4],[2,[8,0]]],[[9,1],[5,[4,[3,2]]]]]").asTreeElement().asInstanceOf[Pair]
      it("finds when not immediate") {
        // [8,0]
        val right = root.left.asInstanceOf[Pair].right.asInstanceOf[Pair].right.findNextLeafRight()
        right.nonEmpty should be(true)
        right.get.value should be(9)
      }

      it("finds when immediate") {
        // [3,4]
        val right = root.left.asInstanceOf[Pair].left.findNextLeafRight()
        right.nonEmpty should be(true)
        right.get.value should be(2)
      }

      it("returns empty when none right") {
        // [3,2]
        val right = root
          .right.asInstanceOf[Pair]
          .right.asInstanceOf[Pair]
          .right.asInstanceOf[Pair]
          .right.findNextLeafRight()
        right.isEmpty should be(true)
      }
    }

    describe("finds left") {
      val root = Parser.parse("[[[3,4],[2,[8,0]]],[[9,1],[5,[4,[3,2]]]]]").asTreeElement().asInstanceOf[Pair]
      it("finds when not immediate") {
        // [[9,1]...
        val left = root.right.asInstanceOf[Pair].left.findNextLeafLeft()
        left.nonEmpty should be(true)
        left.get.value should be(0)
      }

      it("finds when immediate") {
        // [3,2]
        val left = root
          .right.asInstanceOf[Pair]
          .right.asInstanceOf[Pair]
          .right.asInstanceOf[Pair]
          .right.findNextLeafLeft()
        left.nonEmpty should be(true)
        left.get.value should be(4)
      }

      it("returns empty when none left") {
        // [3,4]
        val right = root.left.asInstanceOf[Pair].left.findNextLeafLeft()
        right.isEmpty should be(true)
      }
    }

    it("determines depth") {
      val root = Parser.parse("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]").asTreeElement().asInstanceOf[Pair]
      root.left.asInstanceOf[Pair].right.depth should be (2)
      root.left.asInstanceOf[Pair].right.asInstanceOf[Pair].right.depth should be (3)
      root.right.depth should be (1)
    }

    it("determines depth 4") {
      val root = Parser.parse("[[[[[9,8],1],2],3],4]").asTreeElement().asInstanceOf[Pair]
      root.left.asInstanceOf[Pair].left.asInstanceOf[Pair].left.asInstanceOf[Pair].left.depth should be (4)
    }

    it("explodes pair") {
      val root = Parser.parse("[[6,[5,[4,[3,2]]]],1]").asTreeElement().asInstanceOf[Pair]
      val e = root.left.asInstanceOf[Pair].right.asInstanceOf[Pair].right.asInstanceOf[Pair].right.asInstanceOf[Pair]
      e.parent.isEmpty should be (false)
      e.explode()
      root.toString should be ("[[6,[5,[7,0]]],3]")
    }

    it("iterates left to right seq") {
      val root = Parser.parse("[[[[[9,8],1],2],3],4]").asTreeElement().asInstanceOf[Pair]
      for (i <- root.leftToRightSeq) {
        println(i)
      }
      root.leftToRightSeq.length should be (11)
    }

    it("iterates left to right pairs seq") {
      val root = Parser.parse("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]").asTreeElement().asInstanceOf[Pair]
      for (i <- root.leftToRightPairsSeq) {
        println(i)
      }
      root.leftToRightPairsSeq.length should be (8)
    }

    it("iterates left to right numbers seq") {
      val root = Parser.parse("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]").asTreeElement().asInstanceOf[Pair]
      for (i <- root.leftToRightNumbersSeq) {
        println(i)
      }
      root.leftToRightNumbersSeq.length should be (9)
    }

    describe("reduces") {
      val examples = Table(
        ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
        ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
        ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
        ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
        ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
      )
      it("reduces examples") {
        forAll (examples) { (example: String, result: String) =>
          val root = Parser.parse(example).asTreeElement().asInstanceOf[Pair]
          root.reduce(false)
          root.toString should equal (result)
        }
      }
    }

    it("splits") {
      val root = Parser.parse("[[3,11],5]").asTreeElement().asInstanceOf[Pair]
      root.left.asInstanceOf[Pair].right.asInstanceOf[Number].split()
      root.toString should be ("[[3,[5,6]],5]")
    }

    it("adds then reduces") {
      val e1 = Parser.parse("[[[[4,3],4],4],[7,[[8,4],9]]]").asTreeElement()
      val e2 = Parser.parse("[1,1]").asTreeElement()
      val a = e1 addSimple e2
      a.toString should be ("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
      a.reduce(true)
      a.toString should be ("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    }

    describe("add lines") {
      it("adds lines example 1") {
        val lines =
          """
            |[1,1]
            |[2,2]
            |[3,3]
            |[4,4]
            |""".stripMargin.trim
        Resolver.addLines(Source.fromString(lines).getLines()).toString should equal ("[[[[1,1],[2,2]],[3,3]],[4,4]]")
      }

      it("adds lines example 2") {
        val lines =
          """
            |[1,1]
            |[2,2]
            |[3,3]
            |[4,4]
            |[5,5]
            |""".stripMargin.trim
        Resolver.addLines(Source.fromString(lines).getLines()).toString should equal ("[[[[3,0],[5,3]],[4,4]],[5,5]]")
      }

      it("adds lines example 3") {
        val lines =
          """
            |[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
            |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
            |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
            |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
            |[7,[5,[[3,8],[1,4]]]]
            |[[2,[2,2]],[8,[8,1]]]
            |[2,9]
            |[1,[[[9,3],9],[[9,0],[0,7]]]]
            |[[[5,[7,4]],7],1]
            |[[[[4,2],2],6],[8,7]]
            |""".stripMargin.trim
        Resolver.addLines(Source.fromString(lines).getLines()).toString should
          equal ("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
      }

      describe("calculate magnitude") {
        val examples = Table(
          ("[[1,2],[[3,4],5]]", "143"),
          ("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", "1384"),
          ("[[[[1,1],[2,2]],[3,3]],[4,4]]", "445"),
          ("[[[[3,0],[5,3]],[4,4]],[5,5]]", "791"),
          ("[[[[5,0],[7,4]],[5,5]],[6,6]]", "1137"),
          ("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", "3488"),
        )
        it("calculates magnitudes") {
          forAll (examples) { (pair: String, result: String) =>
            Parser.parse(pair).asTreeElement().magnitude should equal (result.toInt)
          }
        }
      }

      it("solves homework example") {
        val lines =
          """
            |[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
            |[[[5,[2,8]],4],[5,[[9,9],0]]]
            |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
            |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
            |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
            |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
            |[[[[5,4],[7,7]],8],[[8,3],8]]
            |[[9,3],[[9,9],[6,[4,9]]]]
            |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
            |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
            |""".stripMargin.trim
        Resolver.addLines(Source.fromString(lines).getLines()).magnitude should equal (4140)
      }

      it("calculates largest magnitude of any two numbers") {
        val lines =
          """
            |[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
            |[[[5,[2,8]],4],[5,[[9,9],0]]]
            |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
            |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
            |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
            |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
            |[[[[5,4],[7,7]],8],[[8,3],8]]
            |[[9,3],[[9,9],[6,[4,9]]]]
            |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
            |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
            |""".stripMargin.trim
        Resolver.findMax(Source.fromString(lines).getLines()) should equal (3993)
      }
    }
  }

  describe("Day 18") {
    it("a. homework assignment") {
      println(Resolver.addLines(Source.fromResource("day_18.txt").getLines()).magnitude)
    }

    it("b. largest magnitude of any two numbers") {
      println(Resolver.findMax(Source.fromResource("day_18.txt").getLines()))
    }

  }
}
