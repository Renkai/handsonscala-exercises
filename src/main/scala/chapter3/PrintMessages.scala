package chapter3

object PrintMessages {

  class Msg(val id: Int, val parent: Option[Int], val txt: String)

  /**
   * Exercise: Write a recursive method printMessages that can receive an array of Msg class instances,
   * each with an optional parent ID, and use it to print out a threaded fashion.
   * That means that child messages are print out indented underneath their parents,
   * and the nesting can be arbitrarily deep.
   *
   * @param messages
   */
  def printMessages(messages: Array[Msg]): Unit = {
    def iterPrint(parent: Option[Int], indent: Int): Unit = {
      for (message <- messages) {
        if (message.parent == parent) {
          println(s"${" " * indent}#${message.id} ${message.txt}")
          iterPrint(Option(message.id), indent + 1)
        }
      }
    }

    iterPrint(None, 0)
  }

  def main(args: Array[String]): Unit = {
    printMessages(Array(
      new Msg(0, None, "Hello"),
      new Msg(1, Some(0), "World"),
      new Msg(2, None, "I am Cow"),
      new Msg(3, Some(2), "Hear me moo"),
      new Msg(4, Some(2), "Here I stand"),
      new Msg(5, Some(2), "I am Cow"),
      new Msg(6, Some(5), "Here me moo, moo")
    ))
  }
}
