package taller4
import org.scalameter._

object Benchmarking {
  def benchmark[T](name: String)(block: => T): Double = {
    val time = config(
      Key.exec.benchRuns -> 20,
      Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } measure {
      block
    }
    println(s"$name: $time ms")
    time
  }
}
