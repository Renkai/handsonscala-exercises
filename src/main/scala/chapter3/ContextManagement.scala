package chapter3

import java.io.{BufferedReader, BufferedWriter}
import java.nio.file.{Files, Paths}

object ContextManagement {
  def withFileWriter[T](fileName: String)(handler: BufferedWriter => T): T = {
    val output = Files.newBufferedWriter(Paths.get(fileName))
    try handler(output)
    finally output.close()
  }

  def withFileReader[T](fileName: String)(handler: BufferedReader => T): T = {
    val input = Files.newBufferedReader(Paths.get(fileName))
    try handler(input)
    finally input.close()
  }
}
