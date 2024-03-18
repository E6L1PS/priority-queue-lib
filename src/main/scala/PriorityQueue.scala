/**
 * Создан: 16.03.2024.
 *
 * @author Pesternikov Danil
 */
trait PriorityQueue[+E] extends Iterable[E] {

  def enqueue[B >: E](elem: B): PriorityQueue[B]

  def dequeue: (E, PriorityQueue[E])

  def dequeueOption: Option[(E, PriorityQueue[E])]
  
  def peek: E
  
  def peekOption: Option[E]

}
