package taller4
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


class NewtonParalelo {


  object NewtonParalelo {
    sealed trait Expr
    case class Numero(value: Double) extends Expr
    case class Atomo(x: Char) extends Expr
    case class Suma(expr1: Expr, expr2: Expr) extends Expr
    case class Prod(expr1: Expr, expr2: Expr) extends Expr
    case class Resta(expr1: Expr, expr2: Expr) extends Expr
    case class Div(expr1: Expr, expr2: Expr) extends Expr
    case class Expo(expr1: Expr, expr2: Expr) extends Expr
    case class Logaritmo(expr1: Expr) extends Expr

    def limpiar(expr: Expr): Expr = expr match {
      case Suma(e1, e2) =>
        val e1Future = Future { limpiar(e1) }
        val e2Future = Future { limpiar(e2) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Suma(e1Result, e2Result)
      case Prod(e1, e2) =>
        val e1Future = Future { limpiar(e1) }
        val e2Future = Future { limpiar(e2) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Prod(e1Result, e2Result)
      case Resta(e1, e2) =>
        val e1Future = Future { limpiar(e1) }
        val e2Future = Future { limpiar(e2) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Resta(e1Result, e2Result)
      case Div(e1, e2) =>
        val e1Future = Future { limpiar(e1) }
        val e2Future = Future { limpiar(e2) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Div(e1Result, e2Result)
      case Expo(e1, e2) =>
        val e1Future = Future { limpiar(e1) }
        val e2Future = Future { limpiar(e2) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Expo(e1Result, e2Result)
      case Logaritmo(e1) =>
        val e1Future = Future { limpiar(e1) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        Logaritmo(e1Result)
      case _ => expr
    }

    def derivar(expr: Expr, variable: Atomo): Expr = expr match {
      case Suma(e1, e2) =>
        val e1Future = Future { derivar(e1, variable) }
        val e2Future = Future { derivar(e2, variable) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Suma(e1Result, e2Result)
      case Prod(e1, e2) =>
        val e1Future = Future { derivar(e1, variable) }
        val e2Future = Future { derivar(e2, variable) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Suma(Prod(e1Result, e2), Prod(e1, e2Result))
      case Resta(e1, e2) =>
        val e1Future = Future { derivar(e1, variable) }
        val e2Future = Future { derivar(e2, variable) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Resta(e1Result, e2Result)
      case Div(e1, e2) =>
        val e1Future = Future { derivar(e1, variable) }
        val e2Future = Future { derivar(e2, variable) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        Div(Resta(Prod(e1Result, e2), Prod(e1, e2Result)), Prod(e2, e2))
      case Expo(e1, e2) => e2 match {
        case Numero(n) =>
          val e1Future = Future { derivar(e1, variable) }
          val e1Result = Await.result(e1Future, Duration.Inf)
          Prod(Prod(Numero(n), Expo(e1Result, Numero(n - 1))), derivar(e1, variable))
        case _ =>
          val e1Future = Future { derivar(e1, variable) }
          val e2Future = Future { derivar(e2, variable) }
          val e1Result = Await.result(e1Future, Duration.Inf)
          val e2Result = Await.result(e2Future, Duration.Inf)
          Prod(Expo(e1Result, e2Result), Suma(Prod(derivar(e1, variable), Div(e2, e1)), Prod(derivar(e2, variable), Logaritmo(e1))))
      }
      case Logaritmo(e1) =>
        val e1Future = Future { derivar(e1, variable) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        Div(derivar(e1, variable), e1)
      case _ => Numero(0) // Resto de los casos
    }

    def evaluarExpr(expr: Expr, variable: Atomo, valor: Double): Double = expr match {
      case Numero(d) => d
      case Atomo(x) => if (x == variable.x) valor else throw new IllegalArgumentException("Unexpected variable")
      case Suma(e1, e2) =>
        val e1Future = Future { evaluarExpr(e1, variable, valor) }
        val e2Future = Future { evaluarExpr(e2, variable, valor) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        e1Result + e2Result
      case Prod(e1, e2) =>
        val e1Future = Future { evaluarExpr(e1, variable, valor) }
        val e2Future = Future { evaluarExpr(e2, variable, valor) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        e1Result * e2Result
      case Resta(e1, e2) =>
        val e1Future = Future { evaluarExpr(e1, variable, valor) }
        val e2Future = Future { evaluarExpr(e2, variable, valor) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        e1Result - e2Result
      case Div(e1, e2) =>
        val e1Future = Future { evaluarExpr(e1, variable, valor) }
        val e2Future = Future { evaluarExpr(e2, variable, valor) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        e1Result / e2Result
      case Expo(e1, e2) =>
        val e1Future = Future { evaluarExpr(e1, variable, valor) }
        val e2Future = Future { evaluarExpr(e2, variable, valor) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        val e2Result = Await.result(e2Future, Duration.Inf)
        math.pow(e1Result, e2Result)
      case Logaritmo(e1) =>
        val e1Future = Future { evaluarExpr(e1, variable, valor) }
        val e1Result = Await.result(e1Future, Duration.Inf)
        math.log(e1Result)
    }

    def mostrar(e: Expr): String = e match {
      case Numero(d) => d.toString
      case Atomo(x) => x.toString
      case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
      case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
      case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
      case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
      case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
      case Logaritmo(e1) => s"(log(${mostrar(e1)}))"
    }

    def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
      evaluarExpr(f, a, d).abs < 0.001
    }

    def raizNewtonParalelo(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
      val derivadaF = derivar(f, a)

      def raizNewtonRec(x: Double): Double = {
        if (ba(f, a, x)) x
        else {
          val fxTask = Future { evaluarExpr(f, a, x) }
          val dfxTask = Future { evaluarExpr(derivadaF, a, x) }

          val fx = Await.result(fxTask, Duration.Inf)
          val dfx = Await.result(dfxTask, Duration.Inf)

          raizNewtonRec(x - fx / dfx)
        }
      }

      raizNewtonRec(x0)
    }

    def main(args: Array[String]): Unit = {
      val expr1 = Suma(Atomo('x'), Numero(2))
      val expr2 = Prod(Atomo('x'), Atomo('x'))
      val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
      val expr4 = Logaritmo(Atomo('x'))
      val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
      val expr6 = Expo(Atomo('x'), Numero(3))

      println(s"expr1: ${mostrar(expr1)}")
      println(s"evaluar(expr1, Atomo('x'), 5.0): ${evaluarExpr(expr1, Atomo('x'), 5.0)}")
      println(s"expr2: ${mostrar(expr2)}")
      println(s"evaluar(expr2, Atomo('x'), 5.0): ${evaluarExpr(expr2, Atomo('x'), 5.0)}")
      println(s"expr3: ${mostrar(expr3)}")
      println(s"evaluar(expr3, Atomo('x'), 5.0): ${evaluarExpr(expr3, Atomo('x'), 5.0)}")
      println(s"expr4: ${mostrar(expr4)}")
      println(s"evaluar(expr4, Atomo('x'), 5.0): ${evaluarExpr(expr4, Atomo('x'), 5.0)}")
      println(s"expr5: ${mostrar(expr5)}")
      println(s"evaluar(expr5, Atomo('x'), 5.0): ${evaluarExpr(expr5, Atomo('x'), 5.0)}")
      println(s"expr6: ${mostrar(expr6)}")
      println(s"evaluar(expr6, Atomo('x'), 5.0): ${evaluarExpr(expr6, Atomo('x'), 5.0)}")

      val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
      val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
      val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))

      println(s"raizNewton(e1, Atomo('x'), 2.0, buenaAprox): ${raizNewtonParalelo(e1, Atomo('x'), 2.0, buenaAprox)}")
      println(s"raizNewton(e2, Atomo('x'), 2.0, buenaAprox): ${raizNewtonParalelo(e2, Atomo('x'), 2.0, buenaAprox)}")
      println(s"raizNewton(e3, Atomo('x'), 2.0, buenaAprox)}")
    }
  }
}
