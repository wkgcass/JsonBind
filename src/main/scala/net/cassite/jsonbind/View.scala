package net.cassite.jsonbind

import play.api.libs.json.JsValue

/**
 * view of a json
 */
trait View {
  val name: String

  def load(): Unit

  def refresh(): Boolean

  def parse(): JsValue
}
