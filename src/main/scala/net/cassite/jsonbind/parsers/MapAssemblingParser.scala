package net.cassite.jsonbind.parsers

import net.cassite.jsonbind.{AppContext, App, ParsingContext, Parser}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsString, JsObject, JsValue}

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
class MapAssemblingParser extends Parser {
  override def canParse(current: JsValue): Boolean = current match {
    case _: JsObject => true
    case _ => false
  }

  override def parse(current: JsValue, parsingContext: ParsingContext): JsValue = {
    assert(current.isInstanceOf[JsObject])

    MapAssemblingParser.LOGGER.debug("MapAssemblingParser with JsValue:{}", current)

    val map = current.asInstanceOf[JsObject].value
    val parsers = parsingContext.appContext.parsers
    val expParserOpt = parsers.find(p => p.isInstanceOf[ValueParser])
    if (expParserOpt.nonEmpty) {
      val expParser = expParserOpt.get
      val index = parsers.indexOf(expParser)
      expParserOpt.get
      val result = new JsObject(map.map(entry => {
        val newContext = new ParsingContext(parsingContext.$scope, parsingContext.appContext)
        (App.scalaObject(parsingContext.parseExpression(entry._1)).toString, newContext.doNext(entry._2))
      }).filter(p => p._1 != null && p._2 != null))

      MapAssemblingParser.LOGGER.debug("--MapAssemblingParser result is {}", result)

      parsingContext.doNext(result)
    } else throw new IllegalArgumentException("ExpParser not found")
  }
}

object MapAssemblingParser {
  private val LOGGER = LoggerFactory.getLogger(classOf[MapAssemblingParser])
}