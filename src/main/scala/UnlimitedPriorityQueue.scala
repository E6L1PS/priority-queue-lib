import UnlimitedPriorityQueue.CollisionStrategy.{CollisionStrategy, FIFO, LIFO}

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
 * Реализация приоритетной очереди на основе кучи.
 *
 * Куча представлена в виде Vector'а,
 * в котором хранятся объекты представленные Tuple'ом:
 * 1 элемент сам объект,
 * 2 элемент нужен для решения коллизий объектов и представляет собой счетчик (при добавлении объекта счетчик увеличивается).
 * Можно выбрать стратегию решения коллизий объектов LIFO или FIFO, по умолчанию LIFO.
 * Задать приоритет можно используя implicit Ordering.
 *
 * Сложность enqueue: O(log n)
 * Сложность dequeue: O(log n)
 * Сложность peek: O(1)
 * Сложность toList: O(n * log n)
 *
 * Создан: 16.03.2024.
 *
 * @author Pesternikov Danil
 */
class UnlimitedPriorityQueue[+E] protected
(
  strategy: CollisionStrategy,
  heap: Vector[(E, Long)],
  counter: Long
)(implicit ordering: Ordering[E], timeOrdering: Ordering[Long])
  extends PriorityQueue[E] {

  import UnlimitedPriorityQueue.VectorImplicit

  implicit val withTimeOrdering: Ordering[(Any, Long)] = (o1: (Any, Long), o2: (Any, Long)) => {
    val compareResult = ordering.compare(o1._1.asInstanceOf[E], o2._1.asInstanceOf[E])
    val timeComparison = strategy match {
      case FIFO => timeOrdering.compare(o1._2, o2._2)
      case LIFO => timeOrdering.compare(o2._2, o1._2)
    }
    if compareResult == 0 then timeComparison else compareResult
  }

  override def enqueue[B >: E](elem: B): PriorityQueue[B] =
    val time = counter + 1
    UnlimitedPriorityQueue(
      strategy,
      heap.appended((elem, time)).asInstanceOf[Vector[(E, Long)]].siftUp,
      time)

  override def dequeue: (E, PriorityQueue[E]) =
    dequeueOption.getOrElse(throw new NoSuchElementException("Queue is empty"))

  override def dequeueOption: Option[(E, PriorityQueue[E])] =
    heap.headOption.map(tuple =>
      (tuple._1, UnlimitedPriorityQueue(strategy, heap.updated(0, heap.last).dropRight(1).siftDown, counter))
    )

  override def peek: E =
    peekOption.getOrElse(throw new NoSuchElementException("Queue is empty"))

  override def peekOption: Option[E] =
    heap.headOption.map(_._1)

  override def iterator: Iterator[E] = new Iterator[E]:
    private var queue: PriorityQueue[E] = UnlimitedPriorityQueue(strategy, heap, counter)

    override def hasNext: Boolean = queue.peekOption.isDefined

    override def next(): E = {
      if !hasNext then throw new NoSuchElementException("No more elements to iterate")
      val (element, newQueue) = queue.dequeue
      queue = newQueue
      element
    }
}


object UnlimitedPriorityQueue {

  object CollisionStrategy extends Enumeration {
    type CollisionStrategy = Value
    val LIFO, FIFO = Value
  }

  import CollisionStrategy.*

  def apply[E](
                collisionStrategy: CollisionStrategy = CollisionStrategy.LIFO
              )(implicit ordering: Ordering[E]): UnlimitedPriorityQueue[E] =
    new UnlimitedPriorityQueue(collisionStrategy, Vector.empty, Long.MinValue)

  private def apply[E](
                        collisionStrategy: CollisionStrategy,
                        vector: Vector[(E, Long)],
                        counter: Long
                      )(implicit ordering: Ordering[E]): UnlimitedPriorityQueue[E] =
    new UnlimitedPriorityQueue(collisionStrategy, vector, counter)

  implicit class VectorImplicit[E](heap: Vector[(E, Long)])(implicit withTimeOrdering: Ordering[(Any, Long)]) {

    def swapElements(child: Int, parent: Int): Vector[(E, Long)] =
      heap.updated(child, heap(parent)).updated(parent, heap(child))

    def siftUp: Vector[(E, Long)] = siftUp(heap.size - 1)

    def siftUp(idx: Int): Vector[(E, Long)] = {
      @tailrec
      def siftUpRec(heap: Vector[(E, Long)], childIdx: Int): Vector[(E, Long)] = {
        val parentIdx = (childIdx - 1) / 2
        if childIdx == 0 || withTimeOrdering.lt(heap(childIdx), heap(parentIdx)) then heap
        else siftUpRec(heap.swapElements(childIdx, parentIdx), parentIdx)
      }

      siftUpRec(heap, idx)
    }

    def siftDown: Vector[(E, Long)] = {
      @tailrec
      def siftDownRec(heap: Vector[(E, Long)], parentIdx: Int): Vector[(E, Long)] = {
        val leftChildIdx = parentIdx * 2 + 1
        val rightChildIdx = parentIdx * 2 + 2

        var maxIdx = parentIdx
        if leftChildIdx < heap.size && withTimeOrdering.gt(heap(leftChildIdx), heap(maxIdx)) then maxIdx = leftChildIdx
        if rightChildIdx < heap.size && withTimeOrdering.gt(heap(rightChildIdx), heap(maxIdx)) then maxIdx = rightChildIdx

        if maxIdx != parentIdx then siftDownRec(heap.swapElements(parentIdx, maxIdx), maxIdx)
        else heap
      }

      siftDownRec(heap, 0)
    }

  }

}