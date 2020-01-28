package api4s.codegen.ast.idents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RenamerSpec extends AnyFlatSpec with Matchers {
  it should "rename empty identifiers" in {
    val p = "op"
    val r = new Renamer(p, true)

    r.fix("") shouldBe p
    Stream.range(0, 10) foreach { i =>
      r.fix("") shouldBe s"$p$i"
    }
  }

  it should "remove undesired parts of identifier" in {
    val r = new Renamer("p", false)

    r.fix("a-b-c") shouldBe "ABC"
    r.fix("a_b-c") shouldBe "A_bC"
    r.fix("1-abc") shouldBe "P1Abc"
  }
}
