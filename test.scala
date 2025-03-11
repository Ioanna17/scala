object Main extends App {
  println(
    """Лабораторна робота №2
      |Виконала студентка другого курсу Васильчук Іванна КМ-33
      |Варіант 5
      |
      |Умова:
      |1. Написати функцію, щоб перевірити наявність однакових елементів у списку. 
      |   Функція повертає значення "Так" або "Ні".
      |2. Написати функцію для підрахунку суми негативних елементів списку.
      |""".stripMargin)

  def hasDuplicates(list: List[Double]): String = {
    if (list.distinct.size != list.size) "Так" else "Ні"
  }

  def sumOfNegatives(list: List[Double]): Double = {
    list.filter(_ < 0).sum
  }

  def readList: List[Double] = {
    def loop(currentList: List[Double]): List[Double] = {
      print("Введіть одне число(дробові писати через крапку) або написніть [e]nd для завершення вводу: ")
      val input = scala.io.StdIn.readLine()

      if (input.equalsIgnoreCase("e")) {
        return currentList
      }

      try {
        val number = input.toDouble
        val newList = currentList :+ number
        loop(newList)
      } catch {
        case _: NumberFormatException =>
          println(s"Помилка: '$input' не є валідним числом!")
          loop(currentList)
      }
    }

    loop(List.empty)
  }

  println("Введіть елементи списку (раціональні числа):")
  val numbers = readList

  println("\nРезультати:")
  println(s"Список: ${if (numbers.isEmpty) "порожній" else numbers.mkString(", ")}")
  println(s"Наявність дублікатів: ${hasDuplicates(numbers)}")
  println(s"Сума негативних елементів: ${sumOfNegatives(numbers)}")
}
