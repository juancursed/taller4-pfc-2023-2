package taller4

import taller4.Newton._
import taller4.NewtonParalelo._
import taller4.Benchmarking

object Taller4 {
  def main(args: Array[String]): Unit = {
    val expr1 = Suma(Atomo('x'), Numero(1000000))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
    val expr4 = Logaritmo(Atomo('x'))
    val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
    val expr6 = Expo(Atomo('x'), Numero(3))

    val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
    val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
    val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))

    val time1 = Benchmarking.benchmark("Evaluar expr1") {
      evaluarExpr(expr1, Atomo('x'), 5.0)
    }

    val time2 = Benchmarking.benchmark("Raíz de e1") {
      raizNewtonParalelo(e1, Atomo('x'), 2.0, buenaAprox)
    }

    println(s"Tiempo de evaluación de expr1: $time1 ms")
    println(s"Tiempo de cálculo de raíz de e1: $time2 ms")
  }
}