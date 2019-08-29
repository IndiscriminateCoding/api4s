import sbt._

object Boilerplate {
  def core(root: File): Seq[File] = validatedMapN(root) ++ outputs(root)
  def codegen(root: File): Seq[File] = registry(root)

  private val statusRegistry = List(
    (200, "Ok", true),
    (201, "Created", true),
    (202, "Accepted", true),
    (204, "NoContent", false),

    (301, "MovedPermanently", true),
    (302, "Found", true),
    (303, "SeeOther", true),
    (304, "NotModified", false),

    (400, "BadRequest", true),
    (401, "Unauthorized", true),
    (402, "PaymentRequired", true),
    (403, "Forbidden", true),
    (404, "NotFound", true),
    (405, "MethodNotAllowed", true),
    (408, "RequestTimeout", true),

    (500, "InternalServerError", true),
    (502, "BadGateway", true),
    (503, "ServiceUnavailable", true),
    (504, "GatewayTimeout", true)
  )

  private def registry(root: File) = {
    val file = root / "api4s" / "codegen" / "utils" / "Registry.scala"

    IO.write(file, List(
      "package api4s.codegen.utils",
      "",
      "object Registry {",
      "  val registry = Map(",
      statusRegistry.map { case (c, n, a) => s"""    $c -> ("$n", $a)""" }.mkString(",\n"),
      "  )",
      "}"
    ).mkString("\n"))

    Seq(file)
  }

  private def outputs(root: File) = {
    val file = root / "api4s" / "outputs" / "outputs.scala"

    def one(name: String, entityAllowed: Boolean) =
      if (entityAllowed) List(
        s"case class $name[A](content: A)",
        s"object $name {",
        s"  def apply(): $name[Unit] = $name(())",
        "}"
      ) else List(s"case class $name()")

    val res = List("package api4s.outputs", "") ++
      statusRegistry.flatMap { case (_, n, e) => one(n, e) :+ "" }

    IO.write(file, res.mkString("\n"))
    Seq(file)
  }

  private def validatedMapN(root: File) = {
    val file = root / "api4s" / "utils" / "validated" / "MapN.scala"
    val body = {
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
          s"def apply[RR, EE, $typeVarsS]($typedArgsS, $f): ValidatedNec[EE, RR] =",
          s"  ($argsS) match {",
          s"    case (${args.map(c => s"Valid($c)").mkString(", ")}) => Valid(map($argsS))",
          s"    case _ => Invalid(collect($argsS))",
          "  }"
        )
      }

      List.range(1, 22).flatMap(genN)
    }

    IO.write(file, List(
      List(
        "package api4s.utils.validated",
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
}
