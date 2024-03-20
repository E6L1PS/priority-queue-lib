import UnlimitedPriorityQueue.CollisionStrategy
import UnlimitedPriorityQueue.CollisionStrategy.CollisionStrategy

import scala.annotation.tailrec

/**
 * Реализация приоритетной очереди на основе кучи.
 *
 * Переопределен метод enqueue (если прeвышен лимит, проходится по терминальным узлам дерева (в среднем n/2 узлов) для поиска минимального по приоритету)
 *
 * Сложность enqueue: O(log n), в худшем случае (при лимите) O(n)
 *
 * Создан: 16.03.2024.
 *
 * @author Pesternikov Danil
 */
final class LimitedPriorityQueue[+E] private
(
  limit: Long,
  strategy: CollisionStrategy,
  heap: Vector[(E, Long)],
  counter: Long
)(implicit ordering: Ordering[E])
  extends UnlimitedPriorityQueue[E](strategy, heap, counter) {

  import LimitedPriorityQueue.VectorImplicit
  import withTimeOrdering.*

  override def enqueue[B >: E](elem: B): PriorityQueue[B] = {
    val time = counter + 1
    val siftUpedHeap = if heap.size == limit
    then heap.updateLowPriorityElem((elem, time).asInstanceOf[(E, Long)])
    else heap.appended((elem, time)).asInstanceOf[Vector[(E, Long)]].siftUp
    LimitedPriorityQueue(limit, strategy, siftUpedHeap, time)
  }

}


object LimitedPriorityQueue {

  def apply[E](
                limit: Long,
                strategy: CollisionStrategy = CollisionStrategy.LIFO
              )(implicit ordering: Ordering[E]): LimitedPriorityQueue[E] = {
    new LimitedPriorityQueue(limit, strategy, Vector.empty, Long.MinValue)
  }

  private def apply[E](
                        limit: Long,
                        strategy: CollisionStrategy,
                        vector: Vector[(E, Long)],
                        counter: Long
                      )(implicit ordering: Ordering[E]): LimitedPriorityQueue[E] = {
    new LimitedPriorityQueue(limit, strategy, vector, counter)
  }

  implicit private class VectorImplicit[E](heap: Vector[(E, Long)])(implicit withTimeOrdering: Ordering[(Any, Long)]) {

    import UnlimitedPriorityQueue.VectorImplicit

    def siftUp: Vector[(E, Long)] = heap.siftUp

    def updateLowPriorityElem(elem: (E, Long)): Vector[(E, Long)] = {
      @tailrec
      def findLowPriorityIdx(curIdx: Int, lowPriorityIdx: Int, startIdx: Int): Int = {
        if (curIdx < startIdx) lowPriorityIdx
        else {
          val newLowPriorityIdx = if withTimeOrdering.gt(heap(lowPriorityIdx), heap(curIdx))
          then curIdx
          else lowPriorityIdx
          findLowPriorityIdx(curIdx - 1, newLowPriorityIdx, startIdx)
        }
      }

      val lowPriorityElemIdx = findLowPriorityIdx(heap.size - 1, heap.size - 1, heap.size / 2)

      if withTimeOrdering.gt(elem, heap(lowPriorityElemIdx))
      then heap.updated(lowPriorityElemIdx, elem).siftUp(lowPriorityElemIdx)
      else heap
    }

  }

}