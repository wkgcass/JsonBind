package net.cassite.jsonbind.plugins

import java.util.Date

import net.cassite.jsonbind.{PluginContext, Plugin}
import net.cassite.style.util.Utils
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsNumber, JsString, JsValue}

/**
 * plugin for DateFormat<br>
 * example:<br>
 * <code>{{1450760814000 | dateformat('yyyy-mm-dd'}}</code>
 */
object DateFormatPlugin extends Plugin {
  private val LOGGER = LoggerFactory.getLogger(getClass)

  override def isAssignableFrom(func: String): Boolean = func.startsWith("dateformat")

  override def buildFunction(func: String, context: PluginContext): (JsValue) => JsValue = {
    LOGGER.debug("DateFormatPlugin with function : {}", func)

    val params = func.substring("dateformat".length + 1, func.length - 1).split(",").map(_.trim)
    assert(params.length == 1)
    val format = params(0).substring(1, params(0).length - 1)

    LOGGER.debug("\tdateformat(\"{}\")", format)

    (jsValue: JsValue) => {
      assert(jsValue.isInstanceOf[JsNumber])
      new JsString(Utils.$(new Date(jsValue.asInstanceOf[JsNumber].value.longValue())).toString(format))
    }
  }
}
