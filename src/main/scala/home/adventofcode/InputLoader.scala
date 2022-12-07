package home.adventofcode

import scala.io.Source

object InputLoader {

  def loadAsLines(file: String): Iterator[String] =
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(file)).getLines()

  def load(file: String): String =
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(file)).mkString
}
