package chapter6

object BinarySearch {
  def binarySearch[T: Ordering](sorted: IndexedSeq[T], target: T): Boolean = {
    var left = 0
    var right = sorted.length - 1


    while (left <= right) {
      val half = (right + left) / 2

      val halfNode = sorted(half)

      if (halfNode == target) {
        return true
      } else if (Ordering[T].lt(halfNode, target)) {

        left = half + 1
      } else {
        right = half - 1
      }
    }
    false
  }

  def main(args: Array[String]): Unit = {
    assert(binarySearch(Array(1, 3, 7, 9, 13), 3) == true)

    assert(binarySearch(Array(1, 3, 7, 9, 13), 9) == true)

    assert(binarySearch(Array(1, 3, 7, 9, 13), 7) == true)

    assert(binarySearch(Array(1, 3, 7, 9, 13), 8) == false)

    assert(binarySearch(Array(1, 3, 7, 9, 13), 2) == false)

    assert(binarySearch(Array(1, 3, 7, 9, 13), 100) == false)

    assert(binarySearch(Vector("i", "am", "cow", "hear", "me", "moo"), "cow") == true)

    assert(binarySearch(Vector("i", "am", "cow", "hear", "me", "moo"), "moo") == true)

    assert(binarySearch(Vector("i", "am", "cow", "hear", "me", "moo"), "horse") == false)

  }
}
