package net.cassite.jsonbind.parsers

import net.cassite.jsonbind.{App, Parser, ParsingContext, PluginContext}
import play.api.libs.json.{JsString, JsValue}

/**
 * parses expression on values<br>
 * example:<br>
 * <code>
 * {<br>
 * &nbsp;&nbsp;"key":"{{value}}"<br>
 * }<br>
 * </code>
 */
class ValueParser extends Parser {
  override def canParse(current: JsValue): Boolean = current match {
    case JsString(str) => ParsingContext.regex.findFirstMatchIn(str).nonEmpty
    case _ => false
  }

  override def parse(current: JsValue, parsingContext: ParsingContext): JsValue = {
    assert(current.isInstanceOf[JsString])
    parsingContext.parseExpression(current.asInstanceOf[JsString].value)
  }
}