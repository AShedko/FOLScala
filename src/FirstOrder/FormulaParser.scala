package FirstOrder

import FirstOrder.BinOp.BinOp
import fastparse.SingleLineWhitespace._
import fastparse._

object FormulaParser {
  def variable[_: P]: P[Symb] = P(CharIn("a-z").rep(1).!).map(Symb)

  def parens[_: P]: P[Formula] = P("(" ~/ maybeQuantizedExp ~ ")")

  def const[_: P]: P[Const] = P(("0" | "1" | "⊤" | "⟂").!).map(a => Const(a == "1" | a == "⊤"))

  def pred[_: P]: P[Predicate] =
    P((CharIn("A-Z") ~ CharIn("a-zA-Z").rep).! ~/ "(" ~/ variable.rep(sep = ",") ~ ")").map(
      { case (n, s) => Predicate(n, s.toList) }
    )

  def quant[_: P]: P[Quantifier] =
    P(("forall" | "exists" | "∀" | "∃").! ~ variable ~/ "." ~/ rest).map(
      {
        case ("forall" | "∀", symb, formula) => ForAll(symb, formula)
        case ("∃" | "exists", symb, formula) => Exist(symb, formula)
      }
    )

  def negation[_: P]: P[App1] = P(("-" | "¬" | "~") ~ factor).map(App1(Op.Neg, _))

  def factor[_: P]: P[Formula] = P(quant | parens | pred | const | negation)

  def iteratedApp2(optype: BinOp, l: Formula, others: List[Formula]): Formula = others match {
    case r :: rs => App2(optype, l, iteratedApp2(optype, r, rs))
    case (Nil) => l
  }

  def conj[_: P]: P[Formula] = P(factor ~ (("&" | "/\\" | "∧") ~/ factor).rep).map(
    { case (l, rs) => iteratedApp2(BinOp.Conj, l, rs.toList) }
  )

  def term[_: P]: P[Formula] = P(conj ~ (("|" | "\\/" | "∨") ~/ conj).rep).map(
    { case (l, rs) => iteratedApp2(BinOp.Disj, l, rs.toList) }
  )

  def rest[_: P]: P[Formula] = P(term ~ (("->" | "→") ~ term).rep).map(
    { case (l, rs) => iteratedApp2(BinOp.Impl, l, rs.toList) }
  )

  def maybeQuantizedExp[_: P] = P(quant | rest)

  def expr[_: P]: P[Formula] = P(maybeQuantizedExp ~ End)

  def Parse(input: String) =
    parse(input, expr(_))
}
