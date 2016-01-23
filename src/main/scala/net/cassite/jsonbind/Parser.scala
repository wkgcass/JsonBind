package net.cassite.jsonbind

import play.api.libs.json.JsValue

/**
 * parser for json
 */
trait Parser {
  /**
   * determine whether this parser able to parse current json
   * @param current current json to parse
   * @return true if can parse, false otherwise
   */
  def canParse(current: JsValue): Boolean

  /**
   * do parse work
   * @param current current json to parse
   * @param parsingContext context
   * @return parsed js value(cannot be null)
   */
  def parse(current: JsValue, parsingContext: ParsingContext): JsValue
}
