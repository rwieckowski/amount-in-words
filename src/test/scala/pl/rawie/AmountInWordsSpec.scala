package pl.rawie

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class AmountInWordsSpec extends FlatSpec with ShouldMatchers {
  import AmountInWords._

  def amountInWords(n: Int, expected: String) {
    (n inWords) should equal(expected)
  }

  "AmountInWords" should "return amount in words" in {
    amountInWords(0, "zero")
    amountInWords(1, "jeden")
    amountInWords(2, "dwa")
    amountInWords(3, "trzy")
    amountInWords(4, "cztery")
    amountInWords(5, "pięć")
    amountInWords(6, "sześć")
    amountInWords(7, "siedem")
    amountInWords(8, "osiem")
    amountInWords(9, "dziewięć")
    amountInWords(10, "dziesięć")
    amountInWords(11, "jedenaście")
    amountInWords(12, "dwanaście")
    amountInWords(13, "trzynaście")
    amountInWords(14, "czternaście")
    amountInWords(15, "piętnaście")
    amountInWords(16, "szesnaście")
    amountInWords(17, "siedemnaście")
    amountInWords(18, "osiemnaście")
    amountInWords(19, "dziewiętnaście")

    amountInWords(20, "dwadzieścia")
    amountInWords(30, "trzydzieści")
    amountInWords(40, "czterdzieści")
    amountInWords(50, "pięćdziesiąt")
    amountInWords(60, "sześćdziesiąt")
    amountInWords(70, "siedemdziesiąt")
    amountInWords(80, "osiemdziesiąt")
    amountInWords(90, "dziewięćdziesiąt")

    amountInWords(100, "sto")
    amountInWords(200, "dwieście")
    amountInWords(300, "trzysta")
    amountInWords(400, "czterysta")
    amountInWords(500, "pięćset")
    amountInWords(600, "sześćset")
    amountInWords(700, "siedemset")
    amountInWords(800, "osiemset")
    amountInWords(900, "dziewięćset")

    amountInWords(21, "dwadzieścia jeden")
    amountInWords(123, "sto dwadzieścia trzy")

    amountInWords(1000, "jeden tysiąc")
    amountInWords(2000, "dwa tysiące")
    amountInWords(3000, "trzy tysiące")
    amountInWords(4000, "cztery tysiące")
    amountInWords(5000, "pięć tysięcy")

    amountInWords(22000, "dwadzieścia dwa tysiące")
    amountInWords(23000, "dwadzieścia trzy tysiące")
    amountInWords(24000, "dwadzieścia cztery tysiące")

    amountInWords(12000, "dwanaście tysięcy")
    amountInWords(13000, "trzynaście tysięcy")
    amountInWords(14000, "czternaście tysięcy")

    amountInWords(1000000, "jeden milion")

    amountInWords(1234567, "jeden milion dwieście trzydzieści cztery tysiące pięćset sześćdziesiąt siedem")
  }
}
