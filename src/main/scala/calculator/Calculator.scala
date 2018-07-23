package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.keys.toList.map((variable_name) =>
      variable_name -> Signal(eval(
          namedExpressions.get(variable_name).head(),
          namedExpressions - variable_name))).toMap
  }

  def ensureValid(a: Expr, b: Expr, references: Map[String, Signal[Expr]])
                 (fn: (Double, Double) => Double): Double = {
    val first  = eval(a, references)
    val second = eval(b, references)
    if (first == Double.NaN || second == Double.NaN)
      Double.NaN
    else
      fn(first, second)
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v: Double) => v
      case Ref(name: String) => {
        val e = getReferenceExpr(name, references)
        eval(e, references - name)
      }
      case Plus(a: Expr, b: Expr) =>
        ensureValid(a, b, references)((a, b) => a + b)
      case Minus(a: Expr, b: Expr) =>
        ensureValid(a, b, references)((a, b) => a - b)
      case Times(a: Expr, b: Expr) =>
        ensureValid(a, b, references)((a, b) => a * b)
      case Divide(a: Expr, b: Expr) =>
        ensureValid(a, b, references)((a, b) => a / b)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
