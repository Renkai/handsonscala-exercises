package chapter3

object FizzBuzz {
  def fizzBuzz() = {
    for (i <- Range.inclusive(1, 100)) {
      println(
        if (i % 3 == 0 && i % 5 == 0) "FizzBuzz"
        else if (i % 3 == 0) "Fizz"
        else if (i % 5 == 0) "Buzz"
        else i
      )
    }
  }

  def flexibleFizzBuzz(func: String => Unit): Unit = {
    for (i <- Range.inclusive(1, 100)) {
      val fb = if (i % 3 == 0 && i % 5 == 0) "FizzBuzz"
      else if (i % 3 == 0) "Fizz"
      else if (i % 5 == 0) "Buzz"
      else i.toString
      func(fb)
    }
  }
}
