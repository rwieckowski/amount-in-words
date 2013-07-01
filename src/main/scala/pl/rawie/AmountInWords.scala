package pl.rawie

object AmountInWords {
  def apply(n: Int): String = {
    val words = List(
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

    def toWords(n: Int): List[String] = {
      def toWords0(n: Int, xs: List[(Int, String)], as: List[String]): List[String] = xs match {
        case Nil => as
        case _ if n == 0 => as
        case (m, s) :: ys if n >= m => toWords0(n - m, ys, s :: as)
        case _ :: ys => toWords0(n, ys, as)
      }
      toWords0(n, words, Nil)
    }

    def numeral(n: Int, numerals: Array[String]): Option[String] = n match {
      case 0 => None
      case 1 => Some(numerals(0))
      case n if n >= 12 && n <= 14 => Some(numerals(2))
      case n if n % 10 >= 2 && n % 10 <= 4 => Some(numerals(1))
      case _ => Some(numerals(2))
    }

    def inWords0(n: Int, xs: List[(Int, Array[String])], as: List[String]): List[String] = xs match {
      case Nil => toWords(n) ++ as
      case (m, numerals) :: ys if n >= m =>
        inWords0(n % m, ys, Nil ++ numeral(n / m, numerals) ++ toWords(n / m) ++ as)
      case _ :: ys => inWords0(n, ys, as)
    }

    val w = inWords0(n, numerals, Nil)
    if (w.isEmpty) "zero" else w.reverse.mkString(" ")
  }
}
