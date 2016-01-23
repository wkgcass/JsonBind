package net.cassite.jsonbind.parsers

import net.cassite.jsonbind.{ParsingContext, Parser}
import org.slf4j.LoggerFactory
import play.api.libs.json._

/**
 * parses if<br>
 * only if the expression result in <code>JsBoolean(true)</code> or <code>JsString("true")</code>, the node would be appended to json tree<br>
 * --example<br>
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

    IfParser.LOGGER.debug("IfParser with JsValue:{}", current)

    val ifMap = current.asInstanceOf[JsObject].value
    val insideIfMap = ifMap("$if").asInstanceOf[JsObject].value
    val ifKey = insideIfMap.keys.iterator.next()
    val doAppend = parsingContext.parseExpression(ifKey) match {
      case JsBoolean(b) => b
      case JsString(str) => str.toBoolean
      case x => throw new IllegalArgumentException("if only takes booleans or 'true'/'false' strings, but got " + x)
    }

    val newContext = new ParsingContext(parsingContext.$scope, parsingContext.appContext)
    if (doAppend)
      parsingContext.doNext(newContext.doNext(insideIfMap(ifKey)))
    else if (insideIfMap.contains("$else")) {
      parsingContext.doNext(newContext.doNext(insideIfMap("$else")))
    } else null
  }
}

object IfParser {
  private val LOGGER = LoggerFactory.getLogger(classOf[IfParser])
}