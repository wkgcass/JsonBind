package net.cassite.jsonbind.parsers

import net.cassite.jsonbind.{App, ParsingContext, Parser}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsObject, JsValue}

/**
 * assembles a map(JsObject)<br>
 * the parser is used to parse the key<br>
 * example:<br>
 * <code>
 * {<br>
 * &nbsp;&nbsp;"{{key}}":"value"<br>
 * }<br>
 * </code>
 */
object MapAssemblingParser extends Parser {
  private val LOGGER = LoggerFactory.getLogger(getClass)

  override def canParse(current: JsValue): Boolean = current match {
    case _: JsObject => true
    case _ => false
  }

  override def parse(current: JsValue, parsingContext: ParsingContext): JsValue = {
    assert(current.isInstanceOf[JsObject])

    MapAssemblingParser.LOGGER.debug("MapAssemblingParser with JsValue:{}", current)

    val map = current.asInstanceOf[JsObject].value
    val result = new JsObject(map.map(entry => {
      val newContext = new ParsingContext(parsingContext.$scope, parsingContext.appContext)
      (App.scalaObject(parsingContext.parseExpression(entry._1)).toString, newContext.doNext(entry._2))
    }).filter(p => p._1 != null && p._2 != null))

    MapAssemblingParser.LOGGER.debug("\tMapAssemblingParser result is {}", result)

    parsingContext.doNext(result)
  }
}