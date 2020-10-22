package chapter5

object Simplify {

  sealed trait Expr

  case class BinOp(left: Expr, op: String, right: Expr) extends Expr

  case class Literal(value: Int) extends Expr

  case class Variable(name: String) extends Expr

  def stringify(expr: Expr): String = expr match {
    case BinOp(left, op, right) => s"(${stringify(left)} $op ${stringify(right)})"
    case Literal(value) => value.toString
    case Variable(name) => name
  }

  def simplify(expr: Expr): Expr = {
    var res = expr match {
      case BinOp(Literal(x), "+", Literal(y)) => Literal(x + y)
      case BinOp(Literal(x), "-", Literal(y)) => Literal(x - y)
      case BinOp(Literal(x), "*", Literal(y)) => Literal(x * y)
      case BinOp(Literal(x), "/", Literal(y)) => Literal(x / y)
      case BinOp(_, "*", Literal(0)) | BinOp(Literal(0), "*", _) => Literal(0)
      case BinOp(left, "*", Literal(1)) => simplify(left)
      case BinOp(Literal(1), "*", right) => simplify(right)
      case BinOp(Literal(0), "+", e: Expr) => simplify(e)
      case BinOp(Literal(0), "-", e: Expr) => BinOp(Literal(-1), "*", simplify(e))
      case BinOp(e: Expr, "+" | "-", Literal(0)) => simplify(e)
      case BinOp(left,op,right) => BinOp(simplify(left),op,simplify(right))
      case x => x
    }

    if (res == expr) res else simplify(res)

  }
  def main(args: Array[String]): Unit = {
    val example1 = BinOp(Literal(1), "+", Literal(1))

    val str1 = stringify(example1)
    val simple1 = stringify(simplify(example1))
    assert(str1 == "(1 + 1)")
    assert(simple1 == "2")

    val example2 = BinOp(BinOp(Literal(1), "+", Literal(1)), "*", Variable("x"))

    val str2 = stringify(example2)
    val simple2 = stringify(simplify(example2))
    assert(str2 == "((1 + 1) * x)")
    assert(simple2 == "(2 * x)")

    val example3 = BinOp(
      BinOp(Literal(2), "-", Literal(1)),
      "*",
      Variable("x")
    )

    val str3 = stringify(example3)
    val simple3 = stringify(simplify(example3))
    assert(str3 == "((2 - 1) * x)")
    assert(simple3 == "x")

    val example4 = BinOp(
      BinOp(BinOp(Literal(1), "+", (Literal(1))), "*", Variable("y")),
      "+",
      BinOp(BinOp(Literal(1), "-", (Literal(1))), "*", Variable("x"))
    )

    val str4 = stringify(example4)
    val simple4 = stringify(simplify(example4))
    assert(str4 == "(((1 + 1) * y) + ((1 - 1) * x))")
    assert(simple4 == "(2 * y)")
  }
}
