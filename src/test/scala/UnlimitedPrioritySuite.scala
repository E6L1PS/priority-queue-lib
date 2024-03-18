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
    val expected2 = Cat(10, "cat_5")
    assertEquals(obtained2, expected2)

    val q3 = q2.dequeue._2
    val obtained3 = q3.peek
    val expected3 = Cat(10, "cat_2")
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

  test("Тест итерирования по элементам очереди") {
    var catNames: String = ""
    for (
      cat <- catQueue
    ) {
      catNames += cat.name
    }
    assertEquals(catNames, "cat_3cat_5cat_2cat_4cat_1")
  }

  test("Тест преобразования очереди в список и массив") {
    val expectedList = List(
      Cat(1, "cat_1"),
      Cat(10, "cat_2"),
      Cat(11, "cat_3"),
      Cat(5, "cat_4"),
      Cat(10, "cat_5")
    ).sortBy(_.age).reverse
    val expectedArray = Array(
      Cat(1, "cat_1"),
      Cat(10, "cat_2"),
      Cat(11, "cat_3"),
      Cat(5, "cat_4"),
      Cat(10, "cat_5")
    ).sortBy(_.age).reverse
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
