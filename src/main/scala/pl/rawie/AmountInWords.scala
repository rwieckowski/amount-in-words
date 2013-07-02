package pl.rawie

import scala.annotation.tailrec

class AmountInWords(n: Int) {
  def inWords: String = {
    val values = List(
      (900, "dziewięćset"),
      (800, "osiemset"),
      (700, "siedemset"),
      (600, "sześćset"),
      (500, "pięćset"),
      (400, "czterysta"),
      (300, "trzysta"),
      (200, "dwieście"),
      (100, "sto"),
      (90, "dziewięćdziesiąt"),
      (80, "osiemdziesiąt"),
      (70, "siedemdziesiąt"),
      (60, "sześćdziesiąt"),
      (50, "pięćdziesiąt"),
      (40, "czterdzieści"),
      (30, "trzydzieści"),
      (20, "dwadzieścia"),
      (19, "dziewiętnaście"),
      (18, "osiemnaście"),
      (17, "siedemnaście"),
      (16, "szesnaście"),
      (15, "piętnaście"),
      (14, "czternaście"),
      (13, "trzynaście"),
      (12, "dwanaście"),
      (11, "jedenaście"),
      (10, "dziesięć"),
      (9, "dziewięć"),
      (8, "osiem"),
      (7, "siedem"),
      (6, "sześć"),
      (5, "pięć"),
      (4, "cztery"),
      (3, "trzy"),
      (2, "dwa"),
      (1, "jeden")
    )
    val numerals = List(
      (1000000, Array("milion", "miliony", "milionów")),
      (1000, Array("tysiąc", "tysiące", "tysięcy"))
    )

    def numeral(n: Int, numerals: Array[String]): Option[String] = n match {
      case 0 => None
      case 1 => Some(numerals(0))
      case m if m % 10 >= 2 && m % 10 <= 4 && m / 10 != 1 => Some(numerals(1))
      case _ => Some(numerals(2))
    }

    def words(n: Int): List[String] = {
      var i = n
      for ((j, s) <- values if i >= j) yield {i -= j; s}
    }

    var i = n
    val ws = {
      for ((j, ns) <- numerals if i >= j) yield {
        val k = i / j
        i %= j
        words(k) ++ numeral(k, ns)
      }
    }.flatten ++ words(i)

    if (ws.isEmpty) "zero" else ws mkString " "
  }
}

object AmountInWords {
  implicit def intToAmountInWords(n: Int) = new AmountInWords(n)
}