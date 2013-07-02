package pl.rawie

import scala.Range

class AmountInWords(n: Int) {
  private val hundreds = Map(1 -> "sto", 2 -> "dwieście", 3 -> "trzysta", 4 -> "czterysta", 5 -> "pięćset",
    6 -> "sześćset", 7 -> "siedemset", 8 -> "osiemset", 9 -> "dziewięćset")
  private val tens = Map(2 -> "dwadzieścia", 3 -> "trzydzieści", 4 -> "czterdzieści", 5 -> "pięćdziesiąt",
    6 -> "sześćdziesiąt", 7 -> "siedemdziesiąt", 8 -> "osiemdziesiąt", 9 -> "dziewięćdziesiąt")
  private val ones = Map(1 -> "jeden", 2 -> "dwa", 3 -> "trzy", 4 -> "cztery", 5 -> "pięć", 6 -> "sześć", 7 -> "siedem",
    8 -> "osiem", 9 -> "dziewięć", 10 -> "dziesięć", 11 -> "jedenaście", 12 -> "dwanaście", 13 -> "trzynaście",
    14 -> "czternaście", 15 -> "piętnaście", 16 -> "szesnaście", 17 -> "siedemnaście", 18 -> "osiemnaście",
    19 -> "dziewiętnaście")
  private val (one, two_to_four, other) = (1 to 1, 2 to 4, 5 to 19)
  private val numerals = List[(Int, Map[Range, String])](
    (1000000000, Map(one -> "miliard", two_to_four -> "miliardy", other -> "miliardów")),
    (1000000, Map(one -> "milion", two_to_four -> "miliony", other -> "milionów")),
    (1000, Map(one -> "tysiąc", two_to_four -> "tysiące", other -> "tysięcy")),
    (1, Map.empty)
  )

  def inWords: String = {
    def numeral(n: Int, ns: Map[Range, String]): Option[String] =
      ns collectFirst { case (r, num) if r.start <= n && n <= r.end => num }

    val words = numerals.flatMap {
      case (i, ns) => {
        val k = n / i % 1000
        val (h, t, o) = (k / 100, k / 10 % 10, if (k % 100 >= 20) k % 10 else k % 20)
        Nil ++ hundreds.get(h) ++ tens.get(t) ++ ones.get(o) ++ numeral(o, ns)
      }
    }
    if (words.isEmpty) "zero" else words mkString " "
  }
}

object AmountInWords {
  implicit def intToAmountInWords(n: Int) = new AmountInWords(n)
}