package chapter5

object Retry {
  def retry[T](max: Int)(f: => T): T = {
    var tries = 0
    var result: Option[T] = None
    while (result.isEmpty) {
      try {
        result = Some(f)
      }
      catch {
        case e: Throwable =>
          tries += 1
          if (tries > max) throw e
          else {
            println(s"failed, retry #$tries")
          }
      }
    }
    result.get
  }
}

object BackOff {
  def retry[T](max: Int, delay: Int /*milliseconds*/)(f: => T): T = {
    var tries = 0
    var result: Option[T] = None
    var currentDelay = delay
    while (result.isEmpty) {
      try {
        result = Some(f)
      }
      catch {
        case e: Throwable =>
          tries += 1
          if (tries > max) throw e
          else {
            Thread.sleep(delay)
            currentDelay *= 2
            println(s"failed, retry #$tries")
          }
      }
    }
    result.get
  }
}
