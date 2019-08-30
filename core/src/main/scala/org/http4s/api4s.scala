package org.http4s

import org.http4s.util.UrlCodingUtils

// TODO: remove if http4s UrlCodingUtils become public
object api4s {
  def pathEncode(s: String): String = UrlCodingUtils.pathEncode(s)

  def pathDecode(s: String): String = UrlCodingUtils.urlDecode(s)
}
