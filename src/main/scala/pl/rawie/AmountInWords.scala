package pl.rawie

class AmountInWords(n: Int) {
  def inWords: String = {
    val hundreds = Map(1 -> "sto", 2 -> "dwieście", 3 -> "trzysta", 4 -> "czterysta", 5 -> "pięćset", 6 -> "sześćset",
      7 -> "siedemset", 8 -> "osiemset", 9 -> "dziewięćset")
    val tens = Map(2 -> "dwadzieścia", 3 -> "trzydzieści", 4 -> "czterdzieści", 5 -> "pięćdziesiąt",
      6 -> "sześćdziesiąt", 7 -> "siedemdziesiąt", 8 -> "osiemdziesiąt", 9 -> "dziewięćdziesiąt")
    val ones = Map(1 -> "jeden", 2 -> "dwa", 3 -> "trzy", 4 -> "cztery", 5 -> "pięć", 6 -> "sześć", 7 -> "siedem",
      8 -> "osiem", 9 -> "dziewięć", 10 -> "dziesięć", 11 -> "jedenaście", 12 -> "dwanaście", 13 -> "trzynaście",
      14 -> "czternaście", 15 -> "piętnaście", 16 -> "szesnaście", 17 -> "siedemnaście", 18 -> "osiemnaście",
      19 -> "dziewiętnaście")
    val numerals: List[(Int, Map[Symbol, String])] = List(
      (1000000, Map('one -> "milion", 'two_to_four -> "miliony", 'five_to_twenty -> "milionów")),
      (1000, Map('one -> "tysiąc", 'two_to_four -> "tysiące", 'five_to_twenty -> "tysięcy")),
      (1, Map.empty)
    )

    def numeral(n: Int, numerals: Map[Symbol, String]): Option[String] = n match {
      case 0 => numerals.get('zero)
      case 1 => numerals.get('one)
      case m if m % 10 >= 2 && m % 10 <= 4 && m / 10 != 1 => numerals.get('two_to_four)
      case _ => numerals.get('five_to_twenty)
    }

    val words = {
      for ((i, ns) <- numerals) yield {
        val k = n / i % 1000
        val (h, t, o) = (k / 100, k / 10 % 10, if (k % 100 >= 20) k % 10 else k % 20)
        Nil ++ hundreds.get(h) ++ tens.get(t) ++ ones.get(o) ++ numeral(k, ns)
      }
    }.flatten

    if (words.isEmpty) "zero" else words mkString " "
  }
}

object AmountInWords {
  implicit def intToAmountInWords(n: Int) = new AmountInWords(n)
}