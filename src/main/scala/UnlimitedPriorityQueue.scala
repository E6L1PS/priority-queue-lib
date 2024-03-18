import scala.annotation.internal.Child
import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
 * Реализация приоритетной очереди на основе кучи.
 * Создан: 16.03.2024.
 *
 * @author Pesternikov Danil
 */
class UnlimitedPriorityQueue[+E] protected(heap: Vector[E])(implicit ordering: Ordering[E]) extends PriorityQueue[E] {

  import UnlimitedPriorityQueue.VectorImplicit

  override def enqueue[B >: E](elem: B): PriorityQueue[B] =
    UnlimitedPriorityQueue(heap.appended(elem).asInstanceOf[Vector[E]].siftUp)

  override def dequeue: (E, PriorityQueue[E]) =
    dequeueOption.getOrElse(throw new NoSuchElementException("Queue is empty"))

  override def dequeueOption: Option[(E, PriorityQueue[E])] =
    heap.headOption.map((_, new UnlimitedPriorityQueue(heap.updated(0, heap.last).dropRight(1).siftDown)))

  override def peek: E =
    peekOption.getOrElse(throw new NoSuchElementException("Queue is empty"))

  override def peekOption: Option[E] =
    heap.headOption

  override def iterator: Iterator[E] = new Iterator[E]:
    private var queue: PriorityQueue[E] = new UnlimitedPriorityQueue(heap)

    override def hasNext: Boolean = queue.peekOption.isDefined

    override def next(): E = {
      if !hasNext then throw new NoSuchElementException("No more elements to iterate")
      val (element, newQueue) = queue.dequeue
      queue = newQueue
      element
    }

    override def toList: List[E] = super.toList

    override def toArray[B >: E : ClassTag]: Array[B] = super.toArray

}


object UnlimitedPriorityQueue {

  def apply[E]()(implicit ordering: Ordering[E]): UnlimitedPriorityQueue[E] =
    new UnlimitedPriorityQueue(Vector.empty)

  private def apply[E](vector: Vector[E])(implicit ordering: Ordering[E]): UnlimitedPriorityQueue[E] =
    new UnlimitedPriorityQueue(vector)

  implicit class VectorImplicit[E](heap: Vector[E])(implicit ordering: Ordering[E]) {

    def swapElements(child: Int, parent: Int): Vector[E] =
      heap.updated(child, heap(parent)).updated(parent, heap(child))

    def siftUp: Vector[E] = siftUp(heap.size - 1)

    def siftUp(idx: Int): Vector[E] = {
      @tailrec
      def siftUpRec(heap: Vector[E], childIdx: Int): Vector[E] = {
        val parentIdx = (childIdx - 1) / 2
        if childIdx == 0 || ordering.lt(heap(childIdx), heap(parentIdx)) then heap
        else siftUpRec(heap.swapElements(childIdx, parentIdx), parentIdx)
      }

      siftUpRec(heap, idx)
    }

    def siftDown: Vector[E] = {
      @tailrec
      def siftDownRec(heap: Vector[E], parentIdx: Int): Vector[E] = {
        val leftChildIdx = parentIdx * 2 + 1
        val rightChildIdx = parentIdx * 2 + 2

        var maxIdx = parentIdx
        if (leftChildIdx < heap.size && ordering.gt(heap(leftChildIdx), heap(maxIdx))) {
          maxIdx = leftChildIdx
        }
        if (rightChildIdx < heap.size && ordering.gt(heap(rightChildIdx), heap(maxIdx))) {
          maxIdx = rightChildIdx
        }

        if (maxIdx != parentIdx) {
          siftDownRec(heap.updated(parentIdx, heap(maxIdx)).updated(maxIdx, heap(parentIdx)), maxIdx)
        } else {
          heap
        }
      }

      siftDownRec(heap, 0)
    }

  }

}