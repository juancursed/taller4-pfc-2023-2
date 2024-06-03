package taller4

import org.scalameter._
import taller4.Newton
import taller4.NewtonParalelo

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

  def main(args: Array[String]): Unit = {
    val expr1 = Newton.Suma(Newton.Atomo('x'), Newton.Numero(2))
    val expr2 = Newton.Prod(Newton.Atomo('x'), Newton.Atomo('x'))
    val expr3 = Newton.Suma(expr1, Newton.Expo(expr2, Newton.Numero(5)))
    val expr4 = Newton.Logaritmo(Newton.Atomo('x'))
    val expr5 = Newton.Prod(Newton.Div(expr1, expr2), Newton.Resta(expr3, expr4))
    val expr6 = Newton.Expo(Newton.Atomo('x'), Newton.Numero(3))

    val e1 = Newton.Resta(Newton.Prod(Newton.Atomo('x'), Newton.Atomo('x')), Newton.Numero(2.0))
    val e2 = Newton.Resta(Newton.Prod(Newton.Atomo('x'), Newton.Atomo('x')), Newton.Numero(4.0))
    val e3 = Newton.Suma(Newton.Resta(Newton.Prod(Newton.Atomo('x'), Newton.Atomo('x')), Newton.Numero(4.0)), Newton.Prod(Newton.Numero(3.0), Newton.Atomo('x')))

    println("Benchmarking para limpiar")
    benchmark("Secuencial limpiar") {
      (1 to 20).foreach(_ => Newton.limpiar(expr5))
    }

    benchmark("Paralelo limpiar") {
      (1 to 20).foreach(_ => NewtonParalelo.limpiar(expr5))
    }

    println("Benchmarking para derivar")
    benchmark("Secuencial derivar") {
      (1 to 20).foreach(_ => Newton.derivar(e1, Newton.Atomo('x')))
    }

    benchmark("Paralelo derivar") {
      (1 to 20).foreach(_ => NewtonParalelo.derivar(e1, Newton.Atomo('x')))
    }
  }
}

