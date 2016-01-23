package net.cassite.jsonbind.parsers

import net.cassite.jsonbind.{Scope, ParsingContext, Parser}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsArray, JsObject, JsValue}

/**
 * parses foreach<br>
 * example : iteration over an Iterator[_] (usually List[_])<br>
 * <code>
 * {<br>
 * &nbsp;&nbsp;"$foreach":{<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;"x in list":{<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"x":"{{x}}"<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;}<br>
 * &nbsp;&nbsp;}<br>
 * }
 * </code>
 * <br>
 * example : iteration over an Iterator[(_,_)] (usually Map[_,_])<br>
 * <code>
 * {<br>
 * &nbsp;&nbsp;"$foreach":{<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;"(k,v) in map":{<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"{{k}}":"{{x}}"<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;}<br>
 * &nbsp;&nbsp;}<br>
 * }
 * </code>
 */
class ForeachParser extends Parser {
  override def canParse(current: JsValue): Boolean = current match {
    case JsObject(map) => (map contains "$foreach") && map.size == 1
    case _ => false
  }

  override def parse(current: JsValue, parsingContext: ParsingContext): JsValue = {
    assert(current.isInstanceOf[JsObject])

    ForeachParser.LOGGER.debug("ForeachParser with JsValue:{}", current)

    val map = current.asInstanceOf[JsObject].value("$foreach").asInstanceOf[JsObject].value
    val foreachKey = map.keys.iterator.next()
    val XinY = foreachKey.split("in")
    val x = XinY(0).trim
    val y = XinY(1).trim
    val loop = map(foreachKey)

    ForeachParser.LOGGER.debug("--loop info : foreach({} in {}) {}", x, y, loop)

    val seq = if (x.startsWith("(") && x.endsWith(")")) {
      val vars = x.substring(1, x.length - 1).split(",").map(_.trim)
      if (vars.length == 2) {
        for (item <- parsingContext.$scope(y).asInstanceOf[Iterable[(_, _)]]) yield {
          val childScope = new Scope(parsingContext.$scope)
          childScope(vars(0).trim) = item._1
          childScope(vars(1).trim) = item._2
          val parsing = new ParsingContext(childScope, parsingContext.appContext)
          parsing.doNext(loop)
        }
      } else throw new UnsupportedOperationException
    } else {
      for (item <- parsingContext.$scope(y).asInstanceOf[Iterable[_]]) yield {
        val childScope = new Scope(parsingContext.$scope)
        childScope(x) = item
        val parsing = new ParsingContext(childScope, parsingContext.appContext)
        parsing.doNext(loop)
      }
    }
    parsingContext.doNext(new JsArray(seq.toSeq))
  }
}

object ForeachParser {
  private val LOGGER = LoggerFactory.getLogger(classOf[ForeachParser])
}