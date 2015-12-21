package net.cassite.jsonbind

import scala.collection.mutable

/**
 * scope of variables
 */
class Scope(val parent: Scope) {
  private val variables = new mutable.HashMap[String, Any]()

  def update(name: String, any: Any): Unit = variables(name) = any

  def apply(name: String): Any = {
    if ((variables contains name) || parent == null)
      variables(name)
    else
      parent(name)
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
