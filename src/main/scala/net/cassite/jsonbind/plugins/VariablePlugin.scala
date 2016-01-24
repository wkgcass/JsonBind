package net.cassite.jsonbind.plugins

import java.util.NoSuchElementException

import net.cassite.jsonbind.{App, Plugin, PluginContext}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsNumber, JsBoolean, JsString, JsValue}

/**
 * plugin that simply returns a variable<br>
 * let <code>"{{varName}}"</code> string as the JsValue to use the variable defined in scope<br>
 * it supports getter invocation and private field access using dot(<code>.</code>) it will try to find the getter first then go for field<br>
 * if the given value is not a key in $scope, it would try to transform the string into suitable JsValue<br>
 * example<br>
 * <code>"{{value}}"</code><br><code>"{{user.name}}"</code><br><code>"{{true}}"</code> would be translated into JsBoolean(true) if 'true' is not a key in $scope
 */
object VariablePlugin extends Plugin {
  private val LOGGER = LoggerFactory.getLogger(getClass)

  override def isAssignableFrom(func: String): Boolean = !func.contains("(")

  override def buildFunction(func: String, context: PluginContext): (JsValue) => JsValue = {

    LOGGER.debug("VariablePlugin with function : {}", func)

    val str = func.trim
    val v = if (str.startsWith("'") && str.endsWith("'") && str.length >= 2) {
      JsString(str.substring(1, str.length - 1))
    } else {
      try {
        JsBoolean(str.toBoolean)
      } catch {
        case _: Throwable => try {
          JsNumber(str.toDouble)
        } catch {
          case _: Throwable => App.jsonValue(context.$scope(func.trim))
        }
      }
    }
    (x: JsValue) => v
  }
}