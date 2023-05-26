package exercises

import scala.annotation.tailrec

import cats.effect.{Deferred, Fiber, IO, IOApp, Outcome, Ref, Resource}
import cats.effect.implicits._
import cats.syntax.apply._
import cats.syntax.parallel._
import cats.Traverse
import cats.syntax.traverse._
import cats.effect.kernel.Outcome
import cats.instances.list._
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.std.Semaphore
import cats.effect.std.CountDownLatch
import cats.effect.std.CyclicBarrier

// Polymorphic
import cats.Applicative
import cats.Monad
import cats.effect.kernel.Poll
import cats.effect.MonadCancel
import cats.effect.Spawn
import cats.effect.Concurrent
import cats.effect.Temporal
import cats.effect.Sync
import cats.Defer
import cats.effect.Async

import cats.syntax.functor._ // map
import cats.syntax.flatMap._ // flatMap

object basicTree {
  trait BTree[+T] {
    def value: T
    def left: BTree[T]
    def right: BTree[T]
    def isEmpty: Boolean
    def isLeaf: Boolean
    def collectLeaves: List[BTree[T]]
    def leafCount: Int
  }

  case object Bend extends BTree[Nothing] {
    override def value: Nothing = throw new NoSuchElementException
    override def left: BTree[Nothing] = throw new NoSuchElementException
    override def right: BTree[Nothing] = throw new NoSuchElementException
    override def isEmpty: Boolean = true
    override def isLeaf: Boolean = false
    override def collectLeaves: List[BTree[Nothing]] = List()
    override def leafCount: Int = 0
  }

  case class BNode[+T](
      override val value: T,
      override val left: BTree[T],
      override val right: BTree[T])
      extends BTree[T] {
    override def isEmpty: Boolean = false
    override def isLeaf: Boolean = left.isEmpty && right.isEmpty
    override def collectLeaves: List[BTree[T]] = {
      @tailrec
      def collectLeavesAux(todo: List[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = {
        if (todo.isEmpty) acc
        else if (todo.head.isEmpty) collectLeavesAux(todo.tail, acc)
        else if (todo.head.isLeaf) collectLeavesAux(todo.tail, todo.head :: acc)
        else {
          val node = todo.head
          collectLeavesAux(node.left :: node.right :: todo.tail, acc)
        }
      }
      collectLeavesAux(List(this), List())
    }
    override def leafCount: Int = collectLeaves.length

  }
}

object catsTree {
    case class Node[A](value: A, left: Option[Node[A]], right: Option[Node[A]])

    def insert[F[_]: Concurrent, A: Ordering](tree: Option[Node[A]], newValue: A): F[Option[Node[A]]] = ??? // Impossible to parallelize ?
    
    def search[F[_]: Concurrent, A](tree: Option[Node[A]], target: A): F[Option[A]] =
        tree match {
            case None => Concurrent[F].pure(None)
            case Some(node) => 
                if (node.value == target) Concurrent[F].pure(Some(node.value))
                else 
                    (search(node.left, target), search(node.right, target)).parMapN {
                        case (Some(left), _) => Some(left)
                        case (_, Some(right)) => Some(right)
                        case _ => None
                    }
        }
}
object Trees {}
