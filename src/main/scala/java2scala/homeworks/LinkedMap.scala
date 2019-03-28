package java2scala.homeworks
import java2scala.homeworks.LinkedMap.{Cons, Empty}

import scala.annotation.tailrec
import scala.util.Random

sealed trait LinkedMap[K, V] extends Traversable[(K, V)] {

  /** должен вернуть `false` если коллекция содержит хотя бы один элемент */
  override def isEmpty: Boolean = false

  /** должен вернуть `true` если коллекция содержит ключ `key` */
  def contains[A1 >: K](key: A1): Boolean = find { case (k, _) => k == key } match {
    case Some(_) => true
    case None    => false
  }

  /** возвращает Some со значением значения, если коллекция содержит ключ `key`
    * и None если не содержит */
  def apply(key: K): Option[V] = find { case (k, _) => k == key } map {
    case (_, v) => v
  }

  /** возвращает новый LinkedMap[K, V],
    * в котором добавлено или изменено значение для ключа `key` на `value` */
  def update(key: K, value: V): LinkedMap[K, V] =
    LinkedMap(this.toList :+ (key, value): _*)

  /** возвращает новый LinkedMap[K, V]
    * состоящий из тех же позиций, но в обратном порядке */
  def reverse: LinkedMap[K, V] = {
    LinkedMap(this.toList.reverse: _*)
  }

  /** создаёт новый LinkedMap, состоящий из элементов `this` и `other`
    * если какой-то ключ встречается в обеих коллекциях,
    * может быть выбрано любое значение*/
  def ++(other: LinkedMap[K, V]): LinkedMap[K, V] =
    LinkedMap(this.toList ++ other.toList: _*)

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция */
  def mapValues[W](f: V => W): LinkedMap[K, W] =
    LinkedMap(this.map { case (k, v) => k -> f(v) }.toList: _*)

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция,
    * учитывающая ключ*/
  def mapWithKey[W](f: (K, V) => W): LinkedMap[K, W] =
    LinkedMap(this.map { case (k, v) => k -> f(k, v) }.toSeq: _*)

  /** конструирует новый LinkedMap, содеоржащий все записи текущего, кроме заданного ключа */
  def delete(key: K): LinkedMap[K, V] =
    LinkedMap(this.filter(kv => kv._1 != key).toSeq: _*)

  /** применяет действие `action` с побочным эффектом ко всем элементам коллекции */
  @tailrec
  final def foreach[U](action: ((K, V)) => U): Unit = this match {
    case Cons(k, v, Empty()) => action(k, v)
    case Cons(k, v, cons)    => action(k, v); cons.foreach(action)
    case _                   => ()
  }
}

object LinkedMap {

  /** конструирует новый `LinkedMap` на основании приведённых элементов
    * каждый ключ должен присутствовать в результате только один раз
    * если в исходных данныхх ключ встречается несколько раз, может быть
    * выбрано любое из значений
    */
  def apply[K, V](kvs: (K, V)*): LinkedMap[K, V] = {
    @tailrec
    def go(acc: LinkedMap[K, V], kvs: Seq[(K, V)]): LinkedMap[K, V] = kvs match {
      case Nil                               => acc
      case (k, v) :: Nil                     => Cons(k, v, acc)
      case (k, _) :: rest if acc.contains(k) => go(acc, rest)
      case (k, v) :: rest                    => go(Cons(k, v, acc), rest)
    }

    go(Empty[K, V](), kvs.reverse.toList)
  }

  final case class Cons[K, V](key: K, value: V, rest: LinkedMap[K, V]) extends LinkedMap[K, V]

  final case class Empty[K, V]() extends LinkedMap[K, V] {
    override def isEmpty: Boolean                          = true
    override def contains[A1 >: K](key: A1): Boolean       = false
    override def apply(key: K): Option[V]                  = None
    override def update(key: K, value: V): LinkedMap[K, V] = Cons(key, value, this)
    override def reverse: LinkedMap[K, V]                  = this
    override def delete(key: K): LinkedMap[K, V]           = this
  }
}

object LinkedMapDebug extends App {

  val linkedMap1 = LinkedMap()
  val linkedMap2 = LinkedMap("s" -> 1)
  val linkedMap3 = LinkedMap("s" -> 1, "z" -> 2)
  val linkedMap4 = LinkedMap("s" -> 1, "z" -> 2, "z" -> 3, "x" -> 4)
  val linkedMap5 = LinkedMap(List.fill(1000)((Random.alphanumeric.take(5).mkString, Random.nextInt(10000))): _*)

  println(linkedMap1.contains(""))
  println(linkedMap2.contains("s"), linkedMap2.contains("x"))
  println(linkedMap2)
  println(linkedMap3)
  println(linkedMap4)
  println(linkedMap5)
  println()
  println(linkedMap1 ++ linkedMap1)
  println(linkedMap1 ++ linkedMap2)
  println(linkedMap3 ++ linkedMap1)
  println()
  println(linkedMap2 ++ linkedMap2)
  println(linkedMap3 ++ linkedMap3)
  println()
  println(linkedMap4.mapValues(_ + 1000))
  println(linkedMap4.mapWithKey((k, _) => k.toUpperCase))
  println()
  println(linkedMap4.delete("x"))
  println()
  println(linkedMap4.update("x", 5))
  println(linkedMap4.update("b", 6))
  println()
  println(linkedMap4.reverse)

}
