package chapter6

/**
 * Exercise: Define an ImmutableTrie class that has the same methods as the Trie class we discussed in this chapter,
 * but instead of a def add method it should take a sequence of strings during construction and construct
 * the data structure without any use of vars or mutable collections.
 */
class ImmutableTrie(words: Seq[String]) {

  case class Node(hasValue: Boolean,
                  children: Map[Char, Node] = Map.empty)

  private def wordsToNodes(words: Seq[String]): Node = {
    val hasValue = words.exists(_.isEmpty)
    val children =
      words
        .filter(_.nonEmpty).groupBy(_.head).view
        .mapValues(groupedWords => wordsToNodes(groupedWords.map(_.tail))).toMap
    Node(hasValue, children)
  }

  val root = wordsToNodes(words)


  def contains(s: String): Boolean = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    current.exists(_.hasValue)
  }

  def prefixesMatchingString0(s: String): Set[Int] = {
    var current = Option(root)
    val output = Set.newBuilder[Int]
    for ((c, i) <- s.zipWithIndex if current.nonEmpty) {
      if (current.get.hasValue) output += i
      current = current.get.children.get(c)
    }
    if (current.exists(_.hasValue)) output += s.length
    output.result()
  }

  def prefixesMatchingString(s: String): Set[String] = {
    prefixesMatchingString0(s).map(s.substring(0, _))
  }

  def stringsMatchingPrefix(s: String): Set[String] = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c) // initial walk
    if (current.isEmpty) Set()
    else {
      val output = Set.newBuilder[String]

      def recurse(current: Node, path: List[Char]): Unit = {
        if (current.hasValue) output += (s + path.reverse.mkString)
        for ((c, n) <- current.children) recurse(n, c :: path)
      }

      recurse(current.get, Nil) // recursive walk
      output.result()
    }
  }
}

object ImmutableTrie {

  def main(args: Array[String]): Unit = {
    val t = new ImmutableTrie(Seq("mango", "mandarin", "map", "man"))

    assert(t.contains("mango"))

    assert(!t.contains("mang"))

    assert(t.contains("man"))

    assert(t.contains("mandarin"))

    assert(!t.contains("mandarine"))

    assert(t.prefixesMatchingString("mangosteen") == Set("man", "mango"))

    assert(t.stringsMatchingPrefix("man") == Set("man", "mandarin", "mango"))

    assert(t.stringsMatchingPrefix("ma") == Set("map", "man", "mandarin", "mango"))

    assert(t.stringsMatchingPrefix("map") == Set("map"))

    assert(t.stringsMatchingPrefix("mand") == Set("mandarin"))

    assert(t.stringsMatchingPrefix("mando") == Set())
  }

}
