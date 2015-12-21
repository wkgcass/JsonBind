package net.cassite.jsonbind.parsers

import net.cassite.jsonbind.{AppContext, App, ParsingContext, Parser}
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
    val map = current.asInstanceOf[JsObject].value
    val parsers = parsingContext.appContext.parsers
    val expParserOpt = parsers.find(p => p.isInstanceOf[ValueParser])
    if (expParserOpt.nonEmpty) {
      val expParser = expParserOpt.get
      val index = parsers.indexOf(expParser)
      expParserOpt.get
      val newContext = new ParsingContext(parsingContext.$scope, new AppContext(parsingContext.appContext.app, parsingContext.appContext.parsers, parsingContext.appContext.plugins))
      new JsObject(map.map(entry => (App.scalaObject(parsingContext.parseExpression(entry._1)).toString, newContext.doNext(entry._2))).filter(p => p._1 != null && p._2 != null))
    } else throw new IllegalArgumentException("ExpParser not found")
  }
}