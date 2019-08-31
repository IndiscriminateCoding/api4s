package org.http4s

import org.http4s.util.UrlCodingUtils

// TODO: remove if http4s UrlCodingUtils become public
object PathCodecs {
  def encode(s: String): String = UrlCodingUtils.pathEncode(s)

  def decode(s: String): String = UrlCodingUtils.urlDecode(s)
}
