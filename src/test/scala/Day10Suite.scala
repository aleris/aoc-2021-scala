import Day10.{Corrupted, Incomplete}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day10Suite extends AnyFunSpec {
  describe("Day 10 test") {
    it("analyses all lines") {
      val lines = Day10.read(Source.fromResource("day_10_test.txt"))
      val results = Day10.analyse(lines)
      results.size should be (10)
      results.count(_.isInstanceOf[Corrupted]) should be (5)
    }

    it("analyse corrupted lines") {
      Day10.analyseLine("{([(<{}[<>[]}>{[]{[(<()>") should be (Corrupted(']', '}'))
      Day10.analyseLine("[[<[([]))<([[{}[[()]]]") should be (Corrupted(']', ')'))
      Day10.analyseLine("[{[{({}]{}}([{[{{{}}([]") should be (Corrupted(')', ']'))
      Day10.analyseLine("[<(<(<(<{}))><([]([]()") should be (Corrupted('>', ')'))
      Day10.analyseLine("<{([([[(<>()){}]>(<<{{") should be (Corrupted(']', '>'))
    }

    it("scores corrupted lines") {
      val lines = Day10.read(Source.fromResource("day_10_test.txt"))
      val results = Day10.analyse(lines)
      val score = Day10.scoreCorrupted(results)
      score should be (26397)
    }

    it("analyse incomplete lines") {
      Day10.analyseLine("[({(<(())[]>[[{[]{<()<>>") should be(Incomplete("}}]])})]"))
      Day10.analyseLine("[(()[<>])]({[<{<<[]>>(") should be(Incomplete(")}>]})"))
      Day10.analyseLine("(((({<>}<{<{<>}{[]{[]{}") should be(Incomplete("}}>}>))))"))
      Day10.analyseLine("{<[[]]>}<{[{[{[]{()[[[]") should be(Incomplete("]]}}]}]}>"))
      Day10.analyseLine("<{([{{}}[<[[[<>{}]]]>[]]") should be(Incomplete("])}>"))
    }

    it("scores incomplete lines") {
      val lines = Day10.read(Source.fromResource("day_10_test.txt"))
      val results = Day10.analyse(lines)
      Day10.scoreIncomplete(results) should be(288957)
    }
  }

  describe("Day 10") {
    it("a. scores syntax problems") {
      val lines = Day10.read(Source.fromResource("day_10.txt"))
      val results = Day10.analyse(lines)
      val score = Day10.scoreCorrupted(results)
      println(score)
    }

    it("b. scores incomplete lines") {
      val lines = Day10.read(Source.fromResource("day_10.txt"))
      val results = Day10.analyse(lines)
      val score = Day10.scoreIncomplete(results)
      println(score)
    }
  }
}
