package chapter6

object Search {

  def depthSearchPaths[T](start: T, graph: Map[T, Seq[T]]): Map[T, List[T]] = {
    val seen = collection.mutable.Map(start -> List(start))
    val pathLengths = collection.mutable.Map(start -> 0)
    val queue = collection.mutable.Stack((start, List(start), 0))
    while (queue.nonEmpty) {
      val (current, path, pathLength) = queue.pop()
      for {
        next <- graph.getOrElse(current, Nil)
        if !seen.contains(next) && !pathLengths.get(next).exists(_ <= pathLength + 1)
      } {
        val newPath = next :: path
        seen(next) = newPath
        pathLengths(next) = pathLength + 1
        queue.push((next, newPath, pathLength + 1))
      }
    }
    seen.toMap
  }

  def shortestPath[T](start: T, dest: T, graph: Map[T, Seq[T]]): Seq[T] = {
    val shortestReversedPaths = depthSearchPaths(start, graph)
    shortestReversedPaths(dest).reverse
  }

  def main(args: Array[String]): Unit = {
    assert(
      shortestPath(
        start = "a",
        dest = "d",
        graph = Map(
          "a" -> Seq("b", "c"),
          "b" -> Seq("c", "d"),
          "c" -> Seq("d"),
          "d" -> Seq()
        )
      ) == List("a", "c", "d")
    )

    assert(
      shortestPath(
        start = "a",
        dest = "c",
        graph = Map(
          "a" -> Seq("b", "c"),
          "b" -> Seq("c", "d"),
          "c" -> Seq("d"),
          "d" -> Seq()
        )
      ) == List("a", "c")
    )
  }
}
