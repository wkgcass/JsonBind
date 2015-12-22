package net.cassite.jsonbind.plugins

import java.util.NoSuchElementException

import net.cassite.jsonbind.{App, Plugin, PluginContext}
import play.api.libs.json.{JsNumber, JsBoolean, JsString, JsValue}

/**
 * plugin that simply returns a variable<br>
 * let <code>"{{varName}}"</code> string as the JsValue to use the variable defined in scope<br>
 * it supports getter invocation and private field access using dot(<code>.</code>) it will try to find the getter first then go for field<br>
 * if the given value is not a key in $scope, it would try to transform the string into suitable JsValue<br>
 * example<br>
 * <code>"{{value}}"</code><br><code>"{{user.name}}"</code><br><code>"{{true}}"</code> would be translated into JsBoolean(true) if 'true' is not a key in $scope
 */
class VariablePlugin extends Plugin {
  override def isAssignableFrom(func: String): Boolean = !func.contains("(")

  override def buildFunction(func: String, context: PluginContext): (JsValue) => JsValue = {
    val strArr = func.split("\\.").map(_.trim)


    val v = try {
      val any = context.$scope(strArr(0))

      var i = 0
      def recursivelyGetValue(a: Any): Any = {
        i = i + 1
        if (strArr.length == i)
          a
        else {
          val prop = strArr(i)
          val getterName = "get" + prop.charAt(0).toUpper + prop.substring(1)

          recursivelyGetValue(
            try {
              a.getClass.getMethod(getterName).invoke(a)
            } catch {
              case ex: NoSuchMethodException =>
                val field = a.getClass.getDeclaredField(prop)
                field.setAccessible(true)
                field.get(a)
            })
        }
      }
      App.jsonValue(recursivelyGetValue(any))
    } catch {
      case _: NoSuchElementException =>
        val str = strArr(0)
        if (str.startsWith("'") && str.endsWith("'") && str.length >= 2) {
          JsString(str.substring(1, str.length - 1))
        } else {
          try {
            JsBoolean(str.toBoolean)
          } catch {
            case _: Throwable => JsNumber(str.toDouble)
          }
        }
    }
    (x: JsValue) => v
  }
}
