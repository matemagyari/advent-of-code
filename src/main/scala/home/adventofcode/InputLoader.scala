package home.adventofcode

import java.net.URL
import scala.io.Source

object InputLoader {

  def load(url: URL): String = Source.fromURL(url, "UTF-8").mkString

  def loadAsLines(url: URL): Iterator[String] = Source.fromURL(url, "UTF-8").getLines()

  def loadAsLines(file: String): Iterator[String] =
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(file)).getLines()
}
