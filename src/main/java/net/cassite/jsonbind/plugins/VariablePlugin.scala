package net.cassite.jsonbind.plugins

import net.cassite.jsonbind.{App, Plugin, PluginContext}
import play.api.libs.json.JsValue

/**
 * plugin that simply returns a variable<br>
 * let <code>"{{varName}}"</code> string as the JsValue to use the variable defined in scope<br>
 * it supports getter invocation and private field access using dot(<code>.</code>) it will try to find the getter first then go for field<br>
 * example<br>
 * <code>"{{value}}"</code><br><code>"{{user.name}}"</code>
 */
class VariablePlugin extends Plugin {
  override def isAssignableFrom(func: String): Boolean = !func.contains("(")

  override def buildFunction(func: String, context: PluginContext): (JsValue) => JsValue = {
    val strArr = func.split("\\.")
    val any = context.$scope(strArr(0))
    assert(any != null)

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
    val v = App.jsonValue(recursivelyGetValue(any))
    (x: JsValue) => v
  }
}
