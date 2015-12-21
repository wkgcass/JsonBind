package net.cassite.jsonbind.views

import java.io._

import net.cassite.jsonbind.View
import play.api.libs.json.{JsValue, Json}

/**
 * view from a file
 */
class FileView(val name: String, val file: File) extends View {
  private var lastModified = file.lastModified()
  private var text: String = null
  private var parsed: JsValue = null

  private def getStream: InputStream = new FileInputStream(file)

  override def load(): Unit = {
    val reader = new BufferedReader(new InputStreamReader(getStream))
    var str = ""
    var tmp = reader.readLine()
    while (tmp != null) {
      str += tmp
      tmp = reader.readLine()
    }
    text = str
    lastModified = file.lastModified()
    parsed = Json.parse(text)
  }

  override def refresh() =
    if (lastModified != file.lastModified()) {
      load()
      true
    } else false

  def getLastModified = lastModified

  def getText = text

  override def parse(): JsValue = {
    refresh()
    parsed
  }
}

object FileView {
  def apply(name: String, file: File) = new FileView(name, file)
}