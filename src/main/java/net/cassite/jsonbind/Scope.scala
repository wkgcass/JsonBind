package net.cassite.jsonbind

import scala.collection.mutable

/**
 * scope of variables
 */
class Scope(val parent: Scope) {
  private val variables = new mutable.HashMap[String, Any]()

  def update(name: String, any: Any): Unit = variables(name) = any

  /**
   * retrieve the named value from the scope<br>
   * <ol>
   * <li>return the variable if it's directly contained in the scope</li>
   * <li>find the variable in parent scope if parent scope is not null and is contained in parent scope</li>
   * <li>split the given name with dot(<code>.</code>), find the variable with head of the split names, <br>
   * then try to invoke getter or access field to get the value<br>
   * e.g.<br>
   * <code>user.level</code> means find <b>user</b> in this scope, then invoke getter(<code>getLevel()</code>) or access field(<code>level</code>)
   * </li><br>
   * there can be multiple dots
   * </ol>
   * @param name name
   * @return any
   */
  def apply(name: String): Any = {
    if (variables contains name)
      variables(name)
    else if (parent != null && (parent.variables contains name)) parent(name)
    else {
      val strArr = name.split("\\.").map(_.trim)
      if (strArr.length < 2) throw new NoSuchElementException(name)

      val any = if (variables contains strArr(0)) variables(strArr(0)) else parent(strArr(0))

      def recursivelyGetValue(a: Any, i: Int): Any = {
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
            }, i + 1)
        }
      }

      recursivelyGetValue(any, 1)
    }
  }

  def integer(name: String) = apply(name).asInstanceOf[Int]

  def string(name: String) = apply(name).asInstanceOf[String]

  def number(name: String) = apply(name).asInstanceOf[Double]

  def bool(name: String) = apply(name).asInstanceOf[Boolean]

  def list[T](name: String) = apply(name).asInstanceOf[List[T]]

  def map[K, V](name: String) = apply(name).asInstanceOf[Map[K, V]]

  def supplier[T](name: String) = apply(name).asInstanceOf[() => T]

  def consumer[T](name: String) = apply(name).asInstanceOf[T => Unit]

  def func[T, R](name: String) = apply(name).asInstanceOf[T => R]
}
