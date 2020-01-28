package api4s.codegen.ast

import api4s.codegen.ast.Segment._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PathParserSpec extends AnyFlatSpec with Matchers {
  it should "parse non-mixed paths" in {
    PathParser("/a/{b}/c") shouldBe List(Static("a"), Argument("b"), Static("c"))
  }

  it should "parse mixed paths" in {
    PathParser("/abc{def}g{h}") shouldBe
      List(Mixed(List(Static("abc"), Argument("def"), Static("g"), Argument("h"))))

    PathParser("/{abc}def{g}h") shouldBe
      List(Mixed(List(Argument("abc"), Static("def"), Argument("g"), Static("h"))))

    PathParser("/a/{b}c") shouldBe
      List(Static("a"), Mixed(List(Argument("b"), Static("c"))))
  }
}
