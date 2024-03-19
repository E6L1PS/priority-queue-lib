import UnlimitedPriorityQueue.CollisionStrategy

class LimitedPrioritySuite extends munit.FunSuite {

  val queue: PriorityQueue[Animal] = LimitedPriorityQueue[Animal](5)
    .enqueue(Cat(1, "cat_1"))
    .enqueue(Cat(10, "cat_2"))
    .enqueue(Cat(11, "cat_3"))
    .enqueue(Cat(5, "cat_4"))
    .enqueue(Cat(10, "cat_5"))

  test("Тест методов") {
    val q1 = queue

    val obtained = q1.peek
    val expected = Cat(11, "cat_3")
    assertEquals(obtained, expected)

    val q2 = q1.dequeue._2
    val obtained2 = q2.peek
    val expected2 = Cat(10, "cat_5")
    assertEquals(obtained2, expected2)

    val q3 = q2.dequeue._2
    val obtained3 = q3.peek
    val expected3 = Cat(10, "cat_2")
    assertEquals(obtained3, expected3)
  }

  test("Тест превышения лимита") {
    queue
      .enqueue(Cat(2, "cat_6"))
      .enqueue(Cat(5, "cat_7"))
      .enqueue(Cat(3, "cat_8"))

    assertEquals(queue.size, 5)
  }

  test("Тесты лимита") {
    val limit = 6
    val limitedPriorityQueue = LimitedPriorityQueue[Int](limit)
      .enqueue(11)
      .enqueue(10)
      .enqueue(10)
      .enqueue(1)
      .enqueue(5)
      .enqueue(2)
      // после лимита
      .enqueue(3)
      .enqueue(0)
      .enqueue(12)
      .enqueue(-100)
      .enqueue(5)
      .enqueue(6)
      .enqueue(11)

    assertEquals(limitedPriorityQueue.size, limit)
    assertEquals(limitedPriorityQueue.toList, List(12, 11, 11, 10, 10, 6))
  }

  test("Тесты лимита 2") {
    val limit = 5
    val limitedPriorityQueue = LimitedPriorityQueue[Int](limit)
      .enqueue(10)
      .enqueue(6)
      .enqueue(3)
      .enqueue(5)
      .enqueue(5)
      // после лимита
      .enqueue(4)

    assertEquals(limitedPriorityQueue.size, limit)
    assertEquals(limitedPriorityQueue.toList, List(10, 6, 5, 5, 4))
  }

}
