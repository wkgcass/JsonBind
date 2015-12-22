package net.cassite.jsonbind

import play.api.libs.json.{JsString, JsArray, JsObject, JsValue}

/**
 * data and meta data of the json that is in parsing process
 */
class ParsingContext(val $scope: Scope, val appContext: AppContext) {
  val ite = appContext.parsers.iterator

  def doNext(current: JsValue): JsValue = {
    if (ite.hasNext) {
      val parser = ite.next()
      if (parser.canParse(current)) {
        parser.parse(current, this)
      } else {
        doNext(current)
      }
    } else
      current match {
        case JsObject(map) =>
          new JsObject(map.map {
            entry =>
              val subContext = new ParsingContext($scope, appContext)
              (entry._1, subContext.doNext(entry._2))
          }.filter(p => p._1 != null && p._2 != null))
        case JsArray(seq) =>
          new JsArray(seq.map { v =>
            val subContext = new ParsingContext($scope, appContext)
            subContext.doNext(v)
          }.filter(p => p != null))
        case _ => current
      }
  }

  def parseExpression(str: String): JsValue = {
    var result = str
    var jsResult: JsValue = null
    val it = ParsingContext.regex.findAllMatchIn(str)

    val resultMightNotBeJsString = (ParsingContext.regex.findAllMatchIn(str).count(p => true) == 1) && str.startsWith("{{") && str.endsWith("}}")

    for (m <- it;
         string = m.matched;
         matched = string.substring(2, string.length - 2).trim
    ) {
      var last: JsValue = null
      for (exp <- matched.split("\\|").map(_.trim)) {
        val pluginContext = new PluginContext(exp, $scope, appContext)
        last = pluginContext.doNext(last)
      }
      if (resultMightNotBeJsString)
        jsResult = last
      else
        result = ParsingContext.regex.replaceFirstIn(result, App.scalaObject(last).toString)
    }

    if (resultMightNotBeJsString)
      jsResult
    else
      JsString(result)
  }
}

object ParsingContext {
  val regex = """\{\{[^\}\}]+\}\}""".r
}