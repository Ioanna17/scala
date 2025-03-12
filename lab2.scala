import scala.util.matching.Regex
import java.lang.Character.UnicodeBlock

object IntegratedLabWork {
  case class LangStrings(
    enterText: String,
    chooseLanguage: String,
    invalidChoice: String,
    comparison: (Int, Int, String) => String,
    maxWords: Int => String,
    minWords: Int => String,
    noWords: String,
    vowelWordsHeader: String,
    noVowelWords: String
  )

  val englishStrings = LangStrings(
    enterText = "Enter text:",
    chooseLanguage = "Choose language (1 - English, 2 - Українська):",
    invalidChoice = "Invalid choice! Please enter 1 or 2.",
    comparison = (v, c, r) => s"Result: $r (Vowels: $v, Consonants: $c)",
    maxWords = len => s"Longest words ($len letters):",
    minWords = len => s"Shortest words ($len letters):",
    noWords = "No words found",
    vowelWordsHeader = "Words starting and ending with vowel:",
    noVowelWords = "No words starting and ending with vowel"
  )

  val ukrainianStrings = LangStrings(
    enterText = "Введіть текст:",
    chooseLanguage = "Оберіть мову (1 - English, 2 - Українська):",
    invalidChoice = "Невірний вибір! Будь ласка, введіть 1 або 2.",
    comparison = (v, c, r) => s"Результат: $r (Голосні: $v, Приголосні: $c)",
    maxWords = len => s"Слова максимальної довжини ($len символів):",
    minWords = len => s"Слова мінімальної довжини ($len символів):",
    noWords = "Слів не знайдено",
    vowelWordsHeader = "Слова, що починаються і закінчуються голосною:",
    noVowelWords = "Таких слів не знайдено"
  )

  def main(args: Array[String]): Unit = {
    printHeader()
    showMainMenu()
  }

  def printHeader(): Unit = {
    println("Лабораторна робота №2")
    println("Виконала студентка другого курсу Васильчук Іванна КМ-33")
    println("Варіант 5")
    println("\nУмови завдань:")
    println("1. Частина 1:\n1)Написати функцію, щоб перевірити наявність однакових елементів у списку. Функція повертає значення Так або Ні.\n2)Написати функцію для підрахунку суми негативних елементів списку.")
    println("2. Частина 2:\n1)Знайти, яких літер у рядку більше – голосних чи приголосних.\n2)У рядку знайти та вивести на екран усі слова максимальної та мінімальної довжини. \n3)У рядку знайти і надрукувати слова, що починаються і закінчуються голосною літерою.")
    println("3. Вихід")
    println("=" * 50)
  }

  def showMainMenu(): Unit = {
    var continue = true
    while (continue) {
      println("\nГоловне меню:")
      println("1 - Виконати Частину 1 (Список чисел)")
      println("2 - Виконати Частину 2 (Аналіз рядка)")
      println("3 - Вийти")
      print("Ваш вибір: ")

      scala.io.StdIn.readLine() match {
        case "1" => executePart1()
        case "2" => executePart2()
        case "3" => continue = false
        case _   => println("Невірний вибір! Спробуйте знову.")
      }
    }
    println("Програма завершена.")
  }

  def executePart1(): Unit = {
    println("\n=== Частина 1 ===")
    val numbers = readList()
    println("\nРезультати:")
    println(s"Список: ${if (numbers.isEmpty) "порожній" else numbers.mkString(", ")}")
    println(s"Наявність дублікатів: ${if (numbers.distinct.size != numbers.size) "Так" else "Ні"}")
    println(s"Сума негативних елементів: ${numbers.filter(_ < 0).sum}")
  }

  def readList(): List[Double] = {
    def loop(currentList: List[Double]): List[Double] = {
      print("Введіть число або [e]nd для завершення: ")
      val input = scala.io.StdIn.readLine()

      if (input.equalsIgnoreCase("e")) currentList
      else {
        try {
          loop(currentList :+ input.toDouble)
        } catch {
          case _: NumberFormatException =>
            println(s"Помилка: '$input' не є валідним числом!")
            loop(currentList)
        }
      }
    }
    loop(List.empty)
  }

  def executePart2(): Unit = {
    println("\n=== Частина 2 ===")
    var currentLang = chooseLanguage()
    var validInput = false

    while (!validInput) {
      println(currentLang.enterText)
      val input = scala.io.StdIn.readLine()

      if (isValidForLanguage(input, currentLang)) {
        analyzeText(currentLang, input)
        validInput = true
      } else {
        println(if (currentLang == englishStrings)
          "Mixed language characters detected! Please use only one language."
        else
          "Виявлено символи іншої мови! Використовуйте лише одну мову.")
        currentLang = chooseLanguage()
      }
    }
  }

  private def chooseLanguage(): LangStrings = {
    var lang: Option[LangStrings] = None
    while (lang.isEmpty) {
      println("Choose language/Оберіть мову:")
      println("1 - English")
      println("2 - Українська")
      print("> ")

      scala.io.StdIn.readLine() match {
        case "1" => lang = Some(englishStrings)
        case "2" => lang = Some(ukrainianStrings)
        case _ => println("Invalid choice/Невірний вибір! Please enter 1 or 2/Будь ласка, введіть 1 або 2")
      }
    }
    lang.get
  }

  private def isValidForLanguage(text: String, lang: LangStrings): Boolean = {
    val hasEnglish = "[a-zA-Z]".r.findFirstIn(text).isDefined
    val hasUkrainian = "[а-яА-ЯїЇіІєЄґҐщЩ]".r.findFirstIn(text).isDefined

    lang match {
      case `englishStrings` => !hasUkrainian
      case `ukrainianStrings` => !hasEnglish
      case _ => false
    }
  }

  private def analyzeText(lang: LangStrings, input: String): Unit = {
    val vowels = Set(
      'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U',
      'а', 'е', 'и', 'і', 'о', 'у', 'є', 'ю', 'я',
      'А', 'Е', 'И', 'І', 'О', 'У', 'Є', 'Ю', 'Я'
    )

    val cleanedInput = input.replaceAll("\\d", "").trim
    val words = cleanedInput.split("\\s+").filter(_.nonEmpty)

    val (vowelCount, consonantCount) = cleanedInput.foldLeft((0, 0)) {
      case ((v, c), char) =>
        if (vowels.contains(char)) (v + 1, c)
        else if (char.isLetter) (v, c + 1)
        else (v, c)
    }

    val comparisonResult =
      if (vowelCount > consonantCount) "More vowels/Більше голосних"
      else if (consonantCount > vowelCount) "More consonants/Більше приголосних"
      else "Equal/Порівну"

    println(lang.comparison(vowelCount, consonantCount, comparisonResult))

    if (words.nonEmpty) {
      val lengths = words.map(_.length)
      val (maxLen, minLen) = (lengths.max, lengths.min)

      println(lang.maxWords(maxLen))
      println(words.filter(_.length == maxLen).mkString(", "))

      println(lang.minWords(minLen))
      println(words.filter(_.length == minLen).mkString(", "))
    } else {
      println(lang.noWords)
    }

    val vowelWords = words.filter { word =>
      word.nonEmpty &&
        vowels.contains(word.head) &&
        vowels.contains(word.last)
    }

    println(lang.vowelWordsHeader)
    if (vowelWords.nonEmpty) println(vowelWords.mkString(", "))
    else println(lang.noVowelWords)
  }
}