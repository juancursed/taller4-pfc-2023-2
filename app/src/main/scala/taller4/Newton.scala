package taller4

class Newton {
  trait Expr
  case class Numero(d: Double) extends Expr
  case class Atomo(x: Char) extends Expr
  case class Suma(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  case class Resta(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Expo(e1: Expr, e2: Expr) extends Expr
  case class Logaritmo(e1: Expr) extends Expr

  object Newton {
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
      case Suma(Numero(0), e2) => limpiar(e2)
      case Suma(e1, Numero(0)) => limpiar(e1)
      case Suma(e1, e2) => Suma(limpiar(e1), limpiar(e2))
      case Prod(Numero(0), _) => Numero(0)
      case Prod(_, Numero(0)) => Numero(0)
      case Prod(Numero(1), e2) => limpiar(e2)
      case Prod(e1, Numero(1)) => limpiar(e1)
      case Prod(e1, e2) => Prod(limpiar(e1), limpiar(e2))
      case Resta(e1, Numero(0)) => limpiar(e1)
      case Resta(e1, e2) => Resta(limpiar(e1), limpiar(e2))
      case Div(e1, Numero(1)) => limpiar(e1)
      case Div(Numero(0), _) => Numero(0)
      case Div(e1, e2) => Div(limpiar(e1), limpiar(e2))
      case Expo(e1, Numero(0)) => Numero(1)
      case Expo(e1, Numero(1)) => limpiar(e1)
      case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))
      case Logaritmo(e1) => Logaritmo(limpiar(e1))
      case _ => expr
    }

    def derivar(expr: Expr, variable: Atomo): Expr = expr match {
      case Numero(_) => Numero(0)
      case Atomo(x) => if (x == variable.x) Numero(1) else Numero(0)
      case Suma(e1, e2) => Suma(derivar(e1, variable), derivar(e2, variable))
      case Prod(e1, e2) => Suma(Prod(derivar(e1, variable), e2), Prod(e1, derivar(e2, variable)))
      case Resta(e1, e2) => Resta(derivar(e1, variable), derivar(e2, variable))
      case Div(e1, e2) => Div(
        Resta(Prod(derivar(e1, variable), e2), Prod(e1, derivar(e2, variable))),
        Prod(e2, e2)
      )
      case Expo(e1, e2) => e2 match {
        case Numero(n) => Prod(Prod(Numero(n), Expo(e1, Numero(n - 1))), derivar(e1, variable))
        case _ => Prod(
          Expo(e1, e2),
          Suma(
            Prod(derivar(e1, variable), Div(e2, e1)),
            Prod(derivar(e2, variable), Logaritmo(e1))
          )
        )
      }
      case Logaritmo(e1) => Div(derivar(e1, variable), e1)
    }

    def evaluarExpr(expr: Expr, variable: Atomo, valor: Double): Double = expr match {
      case Numero(d) => d
      case Atomo(x) => if (x == variable.x) valor else throw new IllegalArgumentException("Unexpected variable")
      case Suma(e1, e2) => evaluarExpr(e1, variable, valor) + evaluarExpr(e2, variable, valor)
      case Prod(e1, e2) => evaluarExpr(e1, variable, valor) * evaluarExpr(e2, variable, valor)
      case Resta(e1, e2) => evaluarExpr(e1, variable, valor) - evaluarExpr(e2, variable, valor)
      case Div(e1, e2) => evaluarExpr(e1, variable, valor) / evaluarExpr(e2, variable, valor)
      case Expo(e1, e2) => Math.pow(evaluarExpr(e1, variable, valor), evaluarExpr(e2, variable, valor))
      case Logaritmo(e1) => Math.log(evaluarExpr(e1, variable, valor))
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

    def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
      val derivadaF = derivar(f, a)

      def raizNewtonRec(x: Double): Double = {
        if (ba(f, a, x)) x
        else {
          val fx = evaluarExpr(f, a, x)
          val dfx = evaluarExpr(derivadaF, a, x)
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

      println(s"raizNewton(e1, Atomo('x'), 2.0, buenaAprox): ${raizNewton(e1, Atomo('x'), 2.0, buenaAprox)}")
      println(s"raizNewton(e2, Atomo('x'), 2.0, buenaAprox): ${raizNewton(e2, Atomo('x'), 2.0, buenaAprox)}")
      println(s"raizNewton(e3, Atomo('x'), 2.0, buenaAprox)}")
    }
  }
}
