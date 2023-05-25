package exercises

import cats.effect.IO

trait myList[+T] {
  def head: T
  def tail: myList[T]
  def isEmpty: Boolean
}

case object myNil extends myList[Nothing] {

  override def tail: myList[Nothing] = ???

  override def head: Nothing = ???

  override def isEmpty: Boolean = true

}

case class Cons[+T](override val head: T, override val tail: myList[T]) extends myList[T] {
      override def isEmpty: Boolean = false

}

object Lists {}
