package net.cassite.jsonbind

import play.api.libs.json._

import scala.collection.mutable

/**
 * initiation of parsers, plugins, and the central configuration of json
 */
class App(parsers: List[Parser] = Nil, plugins: List[Plugin] = Nil) {
  val context = new AppContext(this, parsers, plugins)

  val nameMap = new mutable.HashMap[String, View]
  val map = new mutable.HashMap[View, Scope]

  def bind(view: View)(init: Scope => Unit): App = {
    if (nameMap contains view.name) throw new IllegalArgumentException(s"duplicated name [$view.name]")
    view.load()
    val scope = new Scope(null)
    init(scope)
    nameMap(view.name) = view
    map(view) = scope
    this
  }

  def view(name: String): JsValue = {
    if (nameMap contains name) {
      val view = nameMap(name)
      view.refresh()
      val parsingContext = new ParsingContext(map(view), context)
      parsingContext.doNext(view.parse())
    }
    else
      throw new IllegalArgumentException(s"view name [$name] not found")
  }
}

object App {
  // TODO
  val defaultConfigurationParser = List()
  val defaultConfigurationPlugin = List()

  def configure() = new App(defaultConfigurationParser, defaultConfigurationPlugin)

  def jsonValue(any: Any): JsValue = any match {
    case s: String => new JsString(s)
    case i: Int => new JsNumber(i)
    case d: Double => new JsNumber(d)
    case b: Boolean => new JsBoolean(b)
    case seq: Seq[_] => new JsArray(seq.map(v => jsonValue(v)))
    case map: Map[_, _] => new JsObject(map.map(entry => (entry._1.toString, jsonValue(entry._2))))
    case null => JsNull
    case _ => throw new IllegalArgumentException
  }

  def scalaObject(js: JsValue): Any = js match {
    case JsString(s) => s
    case JsNumber(n) => if (n.isValidInt) n.intValue() else n.doubleValue()
    case JsBoolean(b) => b
    case JsArray(seq) => seq.map(jsVal => scalaObject(jsVal))
    case JsObject(map) => map.map(entry => (entry._1, scalaObject(entry._2)))
    case JsNull => null
  }
}