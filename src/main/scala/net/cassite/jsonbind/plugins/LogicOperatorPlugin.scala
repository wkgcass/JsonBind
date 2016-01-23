package net.cassite.jsonbind.plugins

import net.cassite.jsonbind.{PluginContext, Plugin}
import play.api.libs.json.{JsBoolean, JsValue}

/**
 * plugin for basic logic operators, including ==,!= or <>, and !<br>
 * example<br>
 * <code>
 * {<br>
 * &nbsp;&nbsp;"key":"{{1==2}}"<br>
 * }<br>
 * </code>
 * would result in <code>{"key":false}</code>
 */
class LogicOperatorPlugin extends Plugin {
  override def isAssignableFrom(func: String): Boolean = func.contains("==") || func.contains("!=") || func.contains("<>") || func.startsWith("!")

  override def buildFunction(func: String, context: PluginContext): (JsValue) => JsValue = {
    // expressions containing == != <> should be split into 2 parts
    // and each part should be parsed to JsValue then compare
    if (func.contains("==") || func.contains("!=") || func.contains("<>")) {
      val spliter = if (func.contains("==")) "==" else if (func.contains("!=")) "!=" else if (func.contains("<>")) "<>" else throw new RuntimeException
      val left = func.substring(0, func.indexOf(spliter))
      val right = func.substring(func.indexOf(spliter) + spliter.length)
      val leftContext = new PluginContext(left, context.$scope, context.appContext)
      val rightContext = new PluginContext(right, context.$scope, context.appContext)

      (jsValue: JsValue) =>
        if (spliter == "==")
          JsBoolean(leftContext.doNext(null) == rightContext.doNext(null))
        else if (spliter == "!=" || spliter == "<>")
          JsBoolean(leftContext.doNext(null) != rightContext.doNext(null))
        else throw new RuntimeException
    }
    // expressions start with ! : the part after ! should be parsed into JsBoolean, then take the opposite boolean value
    else if (func.startsWith("!")) {
      val right = func.substring(1)
      val rightContext = new PluginContext(right, context.$scope, context.appContext)
      (jsValue: JsValue) =>
        rightContext.doNext(null) match {
          case JsBoolean(b) => JsBoolean(!b)
          case _ => throw new IllegalArgumentException("`!` cannot be assigned to non boolean expressions")
        }
    } else throw new UnsupportedOperationException
  }
}
