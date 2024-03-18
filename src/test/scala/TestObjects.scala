/**
 * Создан: 18.03.2024.
 *
 * @author Pesternikov Danil
 */

implicit val orderByAge: Ordering[Animal] = Ordering.by(_.age)
implicit val orderDogByAge: Ordering[Dog] = Ordering.by(_.age)
implicit val orderCatByAge: Ordering[Cat] = Ordering.by(_.age)
abstract class Animal(val age: Int, val name: String)

case class Cat(override val age: Int, override val name: String) extends Animal(age, name)

case class Dog(override val age: Int, override val name: String) extends Animal(age, name)