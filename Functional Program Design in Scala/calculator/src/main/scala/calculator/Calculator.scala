package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    for
      (name, sExpr) <- namedExpressions
    yield
      (name,
      Signal(eval(sExpr(), namedExpressions)))

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    def evalI(trace: Set[Expr], expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
      if trace.contains(expr) then
        Double.NaN
      else
        val newTrace = trace + expr
        expr match
          case Literal(v) => v
          case Ref(name) => evalI(newTrace, getReferenceExpr(name, references), references)
          case Plus(a, b) => evalI(newTrace, a, references) + evalI(newTrace, b, references)
          case Minus(a, b) => evalI(newTrace, a, references) - evalI(newTrace, b, references)
          case Times(a, b) => evalI(newTrace, a, references) * evalI(newTrace, b, references)
          case Divide(a, b) => evalI(newTrace, a, references) / evalI(newTrace, b, references)
    evalI(Set(), expr, references)

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }