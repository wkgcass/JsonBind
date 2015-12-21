package net.cassite.jsonbind

import play.api.libs.json.JsValue

/**
 * context for plugins
 */
class PluginContext(val exp: String, val $scope: Scope, val appContext: AppContext) {
  val ite = appContext.plugins.iterator

  def doNext(current: JsValue): JsValue = {
    if (ite.hasNext) {
      val next = ite.next()
      if (next.isAssignableFrom(exp)) {
        next.buildFunction(exp, this)(current)
      } else doNext(current)
    } else current
  }
}
