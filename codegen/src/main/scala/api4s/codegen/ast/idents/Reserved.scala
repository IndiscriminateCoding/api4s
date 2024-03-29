package api4s.codegen.ast.idents

import api4s.codegen.utils.Registry

object Reserved {
  val keywords: Set[String] = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
    "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null",
    "object", "override", "package", "private", "protected", "return", "sealed", "super", "this",
    "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield"
  )

  val reservedIdentifiers: Set[String] = Set(
    "api", "client", "http4s", "scheme", "authority", "onError", "jsonEncoder"
  )

  val importedSymbols: Set[String] = Set(
    "Map", "String", "Int", "Long", "Double", "Boolean", "Byte", "Some", "Option", "List",
    "Throwable", "Vector", "Set",

    "Json", "Encoder", "Decoder", "Request", "Response", "Status", "Concurrent", "CNil",
    "Resource", "Coproduct", "UnexpectedStatus", "Method", "EntityEncoder", "EntityDecoder", "Inl",
    "Inr", "Uri", "Applicative", "Defer", "NonEmptyChain", "Pure",

    "F", "S", "Runtime", "Endpoint", "Decode", "Errors", "RouteInfo", "MapK",

    "Model", "Http4sServer", "Http4sClient", "Client", "Api", "Media"
  ) ++ Registry.registry.map { case (_, (n, _)) => n }

  def letterDigitOrUnderscore(c: Char): Boolean = c.isLetterOrDigit || c == '_'

  def allowedField(s: String): Boolean =
    s.nonEmpty && s.forall(letterDigitOrUnderscore) && !keywords(s)
}
