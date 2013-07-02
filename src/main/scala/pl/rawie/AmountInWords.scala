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

    def words(n: Int): List[String] = {
      @tailrec
      def words0(n: Int, xs: List[(Int, String)], as: List[String]): List[String] = xs match {
        case Nil => as
        case (m, s) :: ys if n >= m => words0(n - m, ys, s :: as)
        case _ :: ys => words0(n, ys, as)
      }
      words0(n, values, Nil)
    }

    def numeral(n: Int, numerals: Array[String]): Option[String] = n match {
      case 0 => None
      case 1 => Some(numerals(0))
      case m if m >= 12 && m <= 14 => Some(numerals(2))
      case m if m % 10 >= 2 && m % 10 <= 4 => Some(numerals(1))
      case _ => Some(numerals(2))
    }

    @tailrec
    def inWords0(n: Int, xs: List[(Int, Array[String])], as: List[String]): List[String] = xs match {
      case Nil => (words(n) ++ as).reverse
      case (m, ns) :: ys if n >= m =>
        inWords0(n % m, ys, Nil ++ numeral(n / m, ns) ++ words(n / m) ++ as)
      case _ :: ys => inWords0(n, ys, as)
    }

    val ws = inWords0(n, numerals, Nil)
    if (ws.isEmpty) "zero" else ws mkString " "
  }
}

object AmountInWords {
  implicit def intToAmountInWords(n: Int) = new AmountInWords(n)
}