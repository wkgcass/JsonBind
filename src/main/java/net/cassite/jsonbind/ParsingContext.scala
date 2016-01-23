package net.cassite.jsonbind

import play.api.libs.json.{JsString, JsArray, JsObject, JsValue}

/**
 * data and meta data of the json that is in parsing process
 */
class ParsingContext(val $scope: Scope, val appContext: AppContext) {
  val ite = appContext.parsers.iterator

  /**
   * parse the current JsValue with the next parser
   * @param current current JsValue to parse
   * @return parsed JsValue
   */
  def doNext(current: JsValue): JsValue = {
    if (ite.hasNext) {
      val parser = ite.next()
      if (parser.canParse(current)) {
        parser.parse(current, this)
      } else {
        doNext(current)
      }
    } else
    // no more parsers, then parse the inner JsValues if JsObject/JsArray or directly return. null keys/values will be abandoned.
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

  /**
   * parse expression and return the result in JsValue form
   * @param str raw string
   * @return JsValue result
   */
  def parseExpression(str: String): JsValue = {
    var result = str
    var jsResult: JsValue = null
    val it = ParsingContext.regex.findAllMatchIn(str) // all matched {{...}}

    // see whether the result is definitely a string
    // values taken from expressions that starts with '{{' and ends with '}}' might not be JsString
    // but those expressions with multiple '{{...}}' or with non '{{...}}' patterns in it, such as '{{value1}}-{{value2}}' is absolutely JsString
    val resultMightNotBeJsString = (ParsingContext.regex.findAllMatchIn(str).count(p => true) == 1) && str.startsWith("{{") && str.endsWith("}}")

    for (m <- it; // get Match object
         string = m.matched; // get matched string
         matched = string.substring(2, string.length - 2).trim // get expression inside '{{' and '}}'
    ) {
      var last: JsValue = null

      // foreach sub expressions
      for (exp <- matched.split("\\|").map(_.trim)) {
        // instantiate a PluginContext and do parsing
        val pluginContext = new PluginContext(exp, $scope, appContext)
        last = pluginContext.doNext(last)
      }

      if (resultMightNotBeJsString)
        jsResult = last
      else // fill the string with retrieved JsValues
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