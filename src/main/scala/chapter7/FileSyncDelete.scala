package chapter7

object FileSyncDelete {
  def sync(src: os.Path, dest: os.Path) = {
    for (srcSubPath <- os.walk(src)) {
      val subPath = srcSubPath.subRelativeTo(src)
      val destSubPath = dest / subPath
      (os.isDir(srcSubPath), os.isDir(destSubPath)) match {
        case (false, true) | (true, false) =>
          os.copy.over(srcSubPath, destSubPath, createFolders = true)
        case (false, false)
          if !os.exists(destSubPath)
            || !os.read.bytes(srcSubPath).sameElements(os.read.bytes(destSubPath)) =>

          os.copy.over(srcSubPath, destSubPath, createFolders = true)

        case _ => // do nothing
      }
    }

    val originSet = os.walk(src).map(_.relativeTo(src)).toSet

    os.walk(dest).map(_.relativeTo(dest)).foreach {
      destRelPath =>
        if (!originSet.contains(destRelPath)) {
          os.remove(dest / destRelPath)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    println("INITIALIZING SRC AND DEST")
    os.makeDir(os.pwd / "out")
    val src = os.temp.dir(os.pwd / "out")
    val dest = os.temp.dir(os.pwd / "out")

    os.write(src / "folder1" / "hello.txt", "HELLO", createFolders = true)
    os.write(src / "folder1" / "nested" / "world.txt", "world", createFolders = true)

    println("FIRST SYNC")
    sync(src, dest)

    println("FIRST VALIDATION")
    assert(os.read(dest / "folder1" / "hello.txt") == "HELLO")
    assert(os.read(dest /  "folder1" / "nested" / "world.txt") == "world")

    println("UPDATE SRC")
    os.write.over(src / "folder1" / "hello.txt", "hello")
    os.write.over(src / "folder1" / "nested" / "world.txt", "WORLD")

    println("SECOND SYNC")
    sync(src, dest)

    println("SECOND VALIDATION")
    assert(os.read(dest / "folder1" / "hello.txt") == "hello")
    assert(os.read(dest /  "folder1" / "nested" / "world.txt") == "WORLD")

    println("DELETE SRC FILE")
    os.remove(src / "folder1" / "hello.txt")

    println("DELETE SYNC")
    sync(src, dest)

    println("DELETE VALIDATION")
    assert(!os.exists(dest / "folder1" / "hello.txt"))
    assert(os.read(dest /  "folder1" / "nested" / "world.txt") == "WORLD")
  }
}
