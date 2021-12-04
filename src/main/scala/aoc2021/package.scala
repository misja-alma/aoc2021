import cats.effect.IO

import java.io.InputStream
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

package object aoc2021 {
  def scannerFromResource(resourcePath: String): IO[Scanner] = {
    val istream: InputStream = getClass.getResourceAsStream(resourcePath)
    IO.pure(new Scanner(istream, "UTF-8"))
  }

  def scannerToLines(sc: Scanner): Seq[String] = {
    val lineReader = sc.useDelimiter("\n")
    val result = ArrayBuffer[String]()
    while (lineReader.hasNext) result.append(lineReader.next())
    result.toSeq
  }
}
