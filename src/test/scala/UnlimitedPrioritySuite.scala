import UnlimitedPriorityQueue.CollisionStrategy

class UnlimitedPrioritySuite extends munit.FunSuite {

  val catQueue: PriorityQueue[Animal] = UnlimitedPriorityQueue[Animal]()
    .enqueue(Cat(1, "cat_1"))
    .enqueue(Cat(10, "cat_2"))
    .enqueue(Cat(11, "cat_3"))
    .enqueue(Cat(5, "cat_4"))
    .enqueue(Cat(10, "cat_5"))

  test("Тест работы очереди") {
    val q1 = catQueue

    val obtained = q1.peek
    val expected = Cat(11, "cat_3")
    assertEquals(obtained, expected)

    val q2 = q1.dequeue._2
    val obtained2 = q2.peek
    val expected2 = Cat(10, "cat_2")
    assertEquals(obtained2, expected2)

    val q3 = q2.dequeue._2
    val obtained3 = q3.peek
    val expected3 = Cat(10, "cat_5")
    assertEquals(obtained3, expected3)
  }


  test("Тест работы очереди с int") {
    val unlimitedPriorityQueue = UnlimitedPriorityQueue[Int]()
      .enqueue(10)
      .enqueue(11)
      .enqueue(12)
      .enqueue(11)
    println(unlimitedPriorityQueue)
  }

  test("Тест 11") {
    val catQueue: PriorityQueue[Animal] = UnlimitedPriorityQueue[Animal]()
      .enqueue(Cat(1, "cat_1"))
      .enqueue(Cat(2, "cat_2"))
      .enqueue(Cat(3, "cat_3"))
      .enqueue(Cat(4, "cat_4"))
      .enqueue(Cat(5, "cat_5"))
      .enqueue(Cat(6, "cat_6"))
      .enqueue(Cat(7, "cat_7"))
      .enqueue(Cat(8, "cat_8"))
      .enqueue(Cat(9, "cat_9"))
      .enqueue(Cat(10, "cat_10"))


    println(catQueue)
  }

  test("Тест работы очереди с коллизиями в стратегии LIFO") {

    val catQueue1: PriorityQueue[Animal] = UnlimitedPriorityQueue[Animal]()
      .enqueue(Cat(1, "cat_1"))
      .enqueue(Cat(1, "cat_2"))
      .enqueue(Cat(1, "cat_3"))
      .enqueue(Cat(1, "cat_4"))
      .enqueue(Cat(10, "cat_5"))
      .enqueue(Cat(10, "cat_6"))
      .enqueue(Cat(10, "cat_7"))
      .enqueue(Cat(10, "cat_8"))
      .enqueue(Cat(10, "cat_9"))
      .enqueue(Cat(10, "cat_10"))


      .enqueue(Dog(1, "cat_1"))
      .enqueue(Dog(1, "cat_2"))
      .enqueue(Dog(1, "cat_3"))
      .enqueue(Dog(1, "cat_4"))
      .enqueue(Dog(10, "cat_5"))
      .enqueue(Dog(10, "cat_6"))
      .enqueue(Dog(10, "cat_7"))
      .enqueue(Dog(10, "cat_8"))
      .enqueue(Dog(10, "cat_9"))
      .enqueue(Dog(10, "cat_10"))


    println(catQueue1)
  }

  test("Тест работы очереди с коллизиями в стратегии LIFO, явно указав") {
    val queue: PriorityQueue[Animal] = UnlimitedPriorityQueue[Animal](CollisionStrategy.LIFO)
      .enqueue(Cat(4, "cat_1"))
      .enqueue(Cat(4, "cat_2"))
      .enqueue(Cat(4, "cat_3"))
      .enqueue(Cat(4, "cat_4"))
      .enqueue(Cat(4, "cat_5"))
      .enqueue(Cat(5, "cat_6"))
      .enqueue(Cat(5, "cat_7"))
      .enqueue(Cat(5, "cat_8"))
      .enqueue(Cat(5, "cat_9"))
      .enqueue(Cat(5, "cat_10"))

    val expectedList = List(
      Cat(5, "cat_6"),
      Cat(5, "cat_7"),
      Cat(5, "cat_8"),
      Cat(5, "cat_9"),
      Cat(5, "cat_10"),
      Cat(4, "cat_1"),
      Cat(4, "cat_2"),
      Cat(4, "cat_3"),
      Cat(4, "cat_4"),
      Cat(4, "cat_5")
    )

    assertEquals(queue.toList, expectedList)
  }

  test("Тест работы очереди с коллизиями в стратегии FIFO") {
    val catQueue: PriorityQueue[Animal] = UnlimitedPriorityQueue[Animal](CollisionStrategy.FIFO)
      .enqueue(Cat(1, "cat_1"))
      .enqueue(Cat(2, "cat_2"))
      .enqueue(Cat(3, "cat_3"))
      .enqueue(Cat(4, "cat_4"))
      .enqueue(Cat(5, "cat_5"))
      .enqueue(Cat(6, "cat_6"))
      .enqueue(Cat(7, "cat_7"))
      .enqueue(Cat(8, "cat_8"))
      .enqueue(Cat(9, "cat_9"))
      .enqueue(Cat(10, "cat_10"))

      .enqueue(Dog(1, "dog_1"))
      .enqueue(Dog(2, "dog_2"))
      .enqueue(Dog(3, "dog_3"))
      .enqueue(Dog(4, "dog_4"))
      .enqueue(Dog(5, "dog_5"))
      .enqueue(Dog(6, "dog_6"))
      .enqueue(Dog(7, "dog_7"))
      .enqueue(Dog(8, "dog_8"))
      .enqueue(Dog(9, "dog_9"))
      .enqueue(Dog(10, "dog_10"))

    println(catQueue)
  }

  test("Тест методов option") {
    val actual = UnlimitedPriorityQueue[Int]().peekOption.getOrElse(-1)
    val defined = catQueue.dequeueOption.isDefined
    assertEquals(actual, -1)
    assert(defined)
  }

  test("Тест итерирования по элементам очереди") {
    var catNames: String = ""
    for (
      cat <- catQueue
    ) {
      catNames += cat.name
    }
    assertEquals(catNames, "cat_3cat_2cat_5cat_4cat_1")
  }

  test("Тест преобразования очереди в список и массив") {
    val expectedList = List(
      Cat(11, "cat_3"),
      Cat(10, "cat_2"),
      Cat(10, "cat_5"),
      Cat(5, "cat_4"),
      Cat(1, "cat_1")
    )
    val expectedArray = Array(
      Cat(11, "cat_3"),
      Cat(10, "cat_2"),
      Cat(10, "cat_5"),
      Cat(5, "cat_4"),
      Cat(1, "cat_1")
    )

    assertEquals(catQueue.toList, expectedList)
    assertEquals(catQueue.toArray.mkString("Array(", ", ", ")"), expectedArray.mkString("Array(", ", ", ")"))
  }

  test("Тест ковариантности очередей") {
    val dogQueue: PriorityQueue[Dog] = UnlimitedPriorityQueue[Dog]()
      .enqueue(Dog(3, "dog_1"))
      .enqueue(Dog(2, "dog_2"))
      .enqueue(Dog(12, "dog_3"))
      .enqueue(Dog(11, "dog_4"))

    val animalQueue: PriorityQueue[Animal] = dogQueue
    val peek = animalQueue.peek.age
    assert(peek == 12)
  }

}
