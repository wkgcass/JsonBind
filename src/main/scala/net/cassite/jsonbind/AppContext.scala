package net.cassite.jsonbind

/**
 * context for an app
 */
class AppContext(val app: App, val parsers: List[Parser], val plugins: List[Plugin])