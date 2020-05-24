package FirstOrder

object HW {
  def main(args: Array[String]): Unit = {
    val f = ForAll(Symb("a"),
      App2(BinOp.Conj,
      Predicate("A", List(Symb("a"), Symb("b"))),
      Predicate("B", List(Symb("b"))))
    )
    println(f)
    println(Symb("a") == Symb("a"))
    println(Formula.toIndexed(Exist(Symb("b"),f)))
    val g = ForAll(Symb("a"),
    App2(BinOp.Impl,
      Exist(Symb("b"), Predicate("Phi", List(Symb("a"),Symb("b")))),
      Predicate("Psi",List(Symb("a")))
    ))
    println(g)
    val (ind_g, _)  = Formula.toIndexed(g)
    println(ind_g)
    println(IndexedFormula.quantorOut(ind_g))
    val f1 = Formula.randomFormula(3,3, 4)
    println(f1)
    val (f1i, names) = Formula.toIndexed(f1)
    println(IndexedFormula.quantorOut(f1i))
    println(names)
  }
}