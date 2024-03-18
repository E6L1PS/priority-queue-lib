import scala.annotation.tailrec

/**
 * Реализация приоритетной очереди на основе кучи.
 * Создан: 16.03.2024.
 *
 * @author Pesternikov Danil
 */
final class LimitedPriorityQueue[+E] private(limit: Long, heap: Vector[E])(implicit ordering: Ordering[E])
  extends UnlimitedPriorityQueue[E](heap) {

  import LimitedPriorityQueue.VectorImplicit

  override def enqueue[B >: E](elem: B): PriorityQueue[B] = {
    val siftUpedHeap = if heap.size == limit
    then heap.updateLowPriorityElem(elem.asInstanceOf[E])
    else heap.appended(elem).asInstanceOf[Vector[E]].siftUp
    LimitedPriorityQueue(limit, siftUpedHeap)
  }

}


object LimitedPriorityQueue {

  def apply[E](limit: Long)(implicit ordering: Ordering[E]): LimitedPriorityQueue[E] = {
    new LimitedPriorityQueue(limit, Vector.empty)
  }

  private def apply[E](limit: Long, vector: Vector[E])(implicit ordering: Ordering[E]): LimitedPriorityQueue[E] = {
    new LimitedPriorityQueue(limit, vector)
  }

  implicit private class VectorImplicit[E](heap: Vector[E])(implicit ordering: Ordering[E]) {

    import UnlimitedPriorityQueue.VectorImplicit

    def siftUp: Vector[E] = heap.siftUp

    def updateLowPriorityElem(elem: E): Vector[E] = {
      @tailrec
      def findLowPriorityIdx(curIdx: Int, lowPriorityIdx: Int, startIdx: Int): Int = {
        if (curIdx < startIdx) lowPriorityIdx
        else {
          val newLowPriorityIdx = if ordering.gt(heap(lowPriorityIdx), heap(curIdx))
          then curIdx
          else lowPriorityIdx
          findLowPriorityIdx(curIdx - 1, newLowPriorityIdx, startIdx)
        }
      }

      val startIdx = (Math.log(heap.size) / Math.log(2)).toInt
      val lowPriorityElemIdx = findLowPriorityIdx(heap.size - 1, heap.size - 1, startIdx)

      if (ordering.gt(elem, heap(lowPriorityElemIdx)))
        heap.updated(lowPriorityElemIdx, elem).siftUp(lowPriorityElemIdx)
      else
        heap
    }

  }

}