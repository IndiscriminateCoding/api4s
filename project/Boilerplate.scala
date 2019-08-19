import sbt._

object Boilerplate {
  def apply(root: File): Seq[File] = validatedMapN(root)

  private def validatedMapN(root: File) = {
    val file = root / "api4s" / "utils" / "validated" / "MapN.scala"
    IO.write(file, List(
      List(
        "package api4s.utils",
        "",
        "import cats.data.Validated._",
        "import cats.data.{ Chain, NonEmptyChain, ValidatedNec }",
        "",
        "object MapN {",
        "  private def collect[E](xs: ValidatedNec[E, Any]*): NonEmptyChain[E] = {",
        "    var res = Chain.empty[E]",
        "    xs foreach {",
        "      case Invalid(e) => res = res ++ e.toChain",
        "      case _ =>",
        "    }",
        "    NonEmptyChain.fromChainUnsafe(res)",
        "  }",
        ""
      ),
      body.map("  " + _),
      List("}")
    ).flatten.mkString("\n"))
    Seq(file)
  }

  private val body = {
    val lowers = Stream.range('a', 'z')
    val uppers = Stream.range('A', 'Z')

    def genN(n: Int): List[String] = {
      val typeVars = uppers.take(n)
      val typeVarsS = typeVars.mkString(", ")
      val args = lowers.take(n)
      val argsS = args.mkString(", ")
      val typedArgs = args.zip(typeVars).map { case (n, t) => s"$n: ValidatedNec[EE, $t]" }
      val typedArgsS = typedArgs.mkString(", ")
      val f = s"map: (${typeVars.mkString(", ")}) => RR"

      List(
        s"def apply[RR, EE, ${typeVarsS}](${typedArgsS}, $f): ValidatedNec[EE, RR] =",
        s"  ($argsS) match {",
        s"    case (${args.map(c => s"Valid($c)").mkString(", ")}) => Valid(map($argsS))",
        s"    case _ => Invalid(collect($argsS))",
        "  }"
      )
    }

    List.range(1, 22).flatMap(genN)
  }
}
