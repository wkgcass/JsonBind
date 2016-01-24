package net.cassite.jsonbind.parsers

import net.cassite.jsonbind.{Parser, ParsingContext}
import org.slf4j.LoggerFactory
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
object ValueParser extends Parser {
  private val LOGGER = LoggerFactory.getLogger(getClass)

  override def canParse(current: JsValue): Boolean = current match {
    case JsString(str) => ParsingContext.regex.findFirstMatchIn(str).nonEmpty
    case _ => false
  }

  override def parse(current: JsValue, parsingContext: ParsingContext): JsValue = {
    assert(current.isInstanceOf[JsString])

    ValueParser.LOGGER.debug("ValueParser with JsValue:{}", current)

    parsingContext.doNext(parsingContext.parseExpression(current.asInstanceOf[JsString].value))
  }
}