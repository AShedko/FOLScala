
import FirstOrder._
import fastparse.Parsed
import org.scalatest.funsuite._

class FirstOrderTest extends AnyFunSuite {

  test("Symbols are comparable") {
    assert(Symb("a") == Symb("a"))
  }

  val test_expr: ForAll = ForAll(Symb("a"),
    App2(BinOp.Conj,
      Predicate("A", List(Symb("a"), Symb("b"))),
      Predicate("B", List(Symb("b"))))
  )

  test("Indexed Predicates work as intended") {
    assert(Formula.toIndexed(Exist(Symb("b"), test_expr)) ==
      (IExist(IForAll(IApp2(BinOp.Conj,
        IPredicate("A", List(Idx(1), Idx(2))),
        IPredicate("B", List(Idx(2)))))), List("a", "b", "a", "b")))
  }

  test("Parsing works") {
    val formula = FormulaParser.Parse("∀b.∀a.(A(a, b))∧(B(b))").get.value
    assert(formula == ForAll(Symb("b"), test_expr))
  }

  test("Round-trip parsing works") {
    for (i <- 1 to 100) {
      val f1 = Formula.randomFormula(8, 9, 9)
      val parsed = FormulaParser.Parse(f1.toString)
      parsed match {
        case Parsed.Success(value, index) =>
          assert(value == f1)
        case failure: Parsed.Failure =>
          assert(false)
          println(f1)
      }
    }
  }

  test("Prenex form works") {
    val g = ForAll(Symb("a"),
      App2(BinOp.Impl,
        Exist(Symb("b"), Predicate("Phi", List(Symb("a"), Symb("b")))),
        Predicate("Psi", List(Symb("a")))
      ))
    val res = IForAll(IForAll(IApp2(BinOp.Impl, IPredicate("Phi", List(Idx(2), Idx(1))), IPredicate("Psi", List(Idx(2))))))
    val (indexed, names) = Formula.toIndexed(g)
    assert(IndexedFormula.toPrenex(indexed) == res)
    assert(names.toSet == Set("b", "a"))
  }

}
