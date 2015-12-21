package net.cassite.jsonbind.views

import net.cassite.jsonbind.View
import play.api.libs.json.JsValue

/**
 * map view
 */
class JsValueView(val name: String, val jsValue: JsValue) extends View {
  override def load(): Unit = {}

  override def refresh(): Boolean = false

  override def parse(): JsValue = jsValue
}

object JsValueView {
  def apply(name: String, jsValue: JsValue) = new JsValueView(name, jsValue)
}