package net.cassite.jsonbind.views

import java.io.{InputStream, BufferedReader, Reader}

import net.cassite.jsonbind.View
import play.api.libs.json.{JsValue, Json}

/**
 * a view based on a input stream. the view parse it into JsValue, and instantiate a JsValueView
 */
class InputStreamView(val name: String, val stream: InputStream) extends View {
  val view = new JsValueView(name, Json.parse(stream))

  override def load(): Unit = view.load()

  override def refresh(): Boolean = view.refresh()

  override def parse(): JsValue = view.parse()
}

object InputStreamView {
  def apply(name: String, stream: InputStream) = new InputStreamView(name, stream)
}