package net.cassite.jsonbind.parsers

import net.cassite.jsonbind.{ParsingContext, Parser}
import play.api.libs.json._

/**
 * parses if<br>
 * only if the expression result in <code>JsBoolean(true)</code> or <code>JsString("true")</code>, the node would be appended to json tree
 * example<br>
 * <code>
 * {<br>
 * &nbsp;&nbsp;"$if":{<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;"{{bool}}":{<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"key":"value"<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;}<br>
 * &nbsp;&nbsp;}<br>
 * }
 * <code>
 */
class IfParser extends Parser {
  override def canParse(current: JsValue): Boolean = current match {
    case JsObject(map) => (map.size == 1) && (map contains "$if")
    case _ => false
  }

  override def parse(current: JsValue, parsingContext: ParsingContext): JsValue = {
    assert(current.isInstanceOf[JsObject])
    val ifMap = current.asInstanceOf[JsObject].value
    val ifKey = ifMap.keys.iterator.next()
    val doAppend = parsingContext.parseExpression(ifKey) match {
      case JsBoolean(b) => b
      case JsString(str) => str.toBoolean
      case _ => throw new IllegalArgumentException("if only takes booleans or 'true'/'false' strings")
    }
    if (doAppend)
      parsingContext.doNext(ifMap(ifKey))
    else
      null
  }
}
