package net.cassite.jsonbind

import play.api.libs.json.JsValue

/**
 * plugin that resolves values
 */
trait Plugin {
  /**
   * is an instance of the plugin
   * @param func function pattern
   * @return true/false
   */
  def isAssignableFrom(func: String): Boolean

  /**
   * build a function to the args
   * @param func function pattern
   * @return a function takes string as argument and return a string as result
   */
  def buildFunction(func: String, context: PluginContext): JsValue => JsValue
}
