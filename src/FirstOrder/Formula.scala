package FirstOrder

import scala.language.implicitConversions
import scala.util.Random

object BinOp extends Enumeration {

  protected case class Val(repr: String) extends super.Val {}

  implicit def valueToBinOpVal(x: Value): Val = x.asInstanceOf[Val]

  type BinOp = Val
  val Conj = Val("∧")
  val Disj = Val("∨")
  val Impl = Val("→")
  val binOps = Array(Conj, Disj, Impl)
}

object Op extends Enumeration {

  protected case class Val(repr: String) extends super.Val {}

  implicit def valueToBinOpVal(x: Value): Val = x.asInstanceOf[Val]

  type Op = Val
  val I = Val("I")
  val Neg = Val("¬")
}

import FirstOrder.BinOp._
import FirstOrder.Op._

sealed trait Formula

sealed trait Quantifier extends Formula {
  def s: Symb

  def expr: Formula
}

case class ForAll(s: Symb, expr: Formula) extends Quantifier {
  override def toString: String = "∀" + s.n + "." + expr.toString
}

case class Exist(s: Symb, expr: Formula) extends Quantifier {
  override def toString: String = "∃" + s.n + "." + expr.toString
}

case class Symb(n: String) extends Formula {
  override def toString: String = n
}

case class Predicate(name: String, args: List[Symb]) extends Formula {
  override def toString: String =
    name + "(" + args.map(a => a.toString()).mkString(", ") + ")"

}

case class App2(binOp: BinOp, a1: Formula, a2: Formula) extends Formula {
  override def toString: String =
    "(" + a1.toString + ")" + binOp.repr + "(" + a2.toString + ")"
}

case class App1(op: Op, a1: Formula) extends Formula {
  override def toString: String = op.repr + "(" + a1.toString + ")"
}

case class Const(v:Boolean) extends Formula {
  override def toString: String = if (v) "⊤" else "⟂"
}


sealed trait IndexedFormula

sealed trait IndexedQuantifier

sealed trait ISymb extends IndexedFormula

case class IForAll(expr: IndexedFormula) extends IndexedFormula with IndexedQuantifier

case class IExist(expr: IndexedFormula) extends IndexedFormula with IndexedQuantifier

case class Idx(dbIdx: Int) extends IndexedFormula with ISymb

case class IFreeSymb(n: String) extends IndexedFormula with ISymb

case class IPredicate(name: String, args: List[ISymb]) extends IndexedFormula

case class IApp2(binOp: BinOp, a1: IndexedFormula, a2: IndexedFormula) extends IndexedFormula

case class IApp1(op: Op, a1: IndexedFormula) extends IndexedFormula

case class IConst(v : Boolean) extends IndexedFormula

object IndexedFormula {

  def increaseIndex(indexedFormula: IndexedFormula): IndexedFormula = {
    indexedFormula match {
      case IForAll(expr) => IForAll(increaseIndex(expr))
      case IExist(expr) => IExist(increaseIndex(expr))
      case Idx(dbIdx) => Idx(dbIdx + 1)
      case IFreeSymb(n) => indexedFormula
      case IPredicate(name, args) => IPredicate(name, args.map {
        case Idx(idx) => Idx(idx + 1)
        case symb@IFreeSymb(n) => symb
      })
      case IApp2(binOp, a1, a2) => IApp2(binOp, increaseIndex(a1), increaseIndex(a2))
      case IApp1(op, a1) => IApp1(op, increaseIndex(a1))
      case IConst(v) => indexedFormula
    }
  }

  def quantorOut(indexedFormula: IndexedFormula):IndexedFormula = {
    indexedFormula match {
      case IApp2(op, f1, f2) =>
        op match {
          case Conj | Disj =>
            f1 match {
              case IExist(expr) => IExist(IApp2(op, expr, increaseIndex(f2)))
              case IForAll(expr) => IForAll(IApp2(op, expr, increaseIndex(f2)))
              case default =>
                f2 match {
                  case IExist(expr) => IExist(IApp2(op, increaseIndex(f1), expr))
                  case IForAll(expr) => IForAll(IApp2(op, increaseIndex(f1), expr))
                  case _ => IApp2(op, quantorOut(f1), quantorOut(f2))
                }
            }
          case Impl =>
            f1 match {
              case IExist(expr) => IForAll(IApp2(op, expr, increaseIndex(f2)))
              case IForAll(expr) => IExist(IApp2(op, expr, increaseIndex(f2)))
              case default =>
                f2 match {
                  case IExist(expr) => IExist(IApp2(op, increaseIndex(f1), expr))
                  case IForAll(expr) => IForAll(IApp2(op, increaseIndex(f1), expr))
                  case _ => IApp2(op, quantorOut(f1), quantorOut(f2))
                }
            }
        }
      case IApp1(op, a1) =>
        op match {
          case I => IApp1(op, IApp1(op, a1))
          case Neg => a1 match {
            case IExist(expr) => IForAll(IApp1(op, quantorOut(expr)))
            case IForAll(expr) => IExist(IApp1(op, quantorOut(expr)))
            case ff: IndexedFormula => IApp1(Neg, quantorOut(ff))
          }
        }
      case IExist(expr) => IExist(quantorOut(expr))
      case IForAll(expr) => IForAll(quantorOut(expr))
      case _ => indexedFormula
    }
  }

  def toPrenex(indexedFormula: IndexedFormula): IndexedFormula = {
    var orig, res = indexedFormula
    var trip = 0
    do {
      orig = res
      res = quantorOut(orig)
    } while (res != orig)
    res
  }
}

object Formula {
  def toIndexed(formula: Formula): (IndexedFormula, List[String]) = {
    def depth(s:List[String], needle:String):Int = {
      var c = 0
      for (v <- s) {
        c += 1
        if (v == needle) return c
      }
      0
    }
    def symbF(n: String, stor: List[String]): (ISymb, List[String]) = {
      val d = depth(stor,n)
      val v = if (d!=0)
        Idx(d)
      else
        IFreeSymb(n)
      (v, stor)
    }

    def toIndexedInner(formula: Formula, stor: List[String]):(IndexedFormula,List[String]) = {
      formula match {
        case ForAll(s, expr) => val (f,st) =toIndexedInner(expr, s.n :: stor); (IForAll(f),st)
        case Exist(s, expr) => val (f,st) = toIndexedInner(expr, s.n :: stor); (IExist(f),st)
        case Symb(n) => symbF(n, stor)
        case Predicate(name, args) => (IPredicate(name, args.map(s => symbF(s.n, stor)._1)), stor)
        case App2(binOp, a1, a2) =>
          val i1 = toIndexedInner(a1, stor)
          val i2 = toIndexedInner(a2, stor)
          (IApp2(binOp, i1._1, i2._1), i1._2 ++ i2._2)
        case App1(op, a1) => val f = toIndexedInner(a1, stor); (IApp1(op, f._1), f._2)
        case Const(v) => (IConst(v),stor)
      }
    }

    toIndexedInner(formula, List())
  }

  def getRandElemOf[T](arr: Array[T]):T ={ arr(Random.nextInt(arr.length)) }

  def safeGetRandElemOf[T](arr: Array[T]): Option[T] =
    if (arr.isEmpty) None
    else Some(getRandElemOf(arr))

  def randomFromSet[T](s: Set[T]): Option[T] = {
    if (s.isEmpty) None
    else {
      val n = Random.nextInt(s.size)
      Some(s.iterator.drop(n).next)
    }
  }

  def randomFormula(num_vars:Int, num_preds:Int, depth: Int) :Formula = {
    object FormulaMaker {
      private val vars = Random.shuffle(('a' to 'z').toList).take(num_vars).map((a: Char) => a.toString).toArray
      private var current_var = 0;

      private val preds = Random.shuffle(('A' to 'Z').toList).take(num_preds).map((a: Char) => a.toString).toArray.
        zip(List.fill(num_preds)(Random.between(1, 3)))

      class ExprFactory {
        private var free = vars.toSet
        private var bound = Array[String]();

        def make(depth: Int): Formula = {
          if (depth > 0)
            builders(Random.between(0, builders.length))(this, depth)
          else
            predBuilder(this) // Just make a predicate
        }

        def add_binder():Option[Symb]={
          val n = randomFromSet(free)
          n.map((x:String) => {
            free = free.removedAll(Seq(x))
            bound = bound.appended(x)
            Symb(x)
          })
        }

        def sym_builder(): Symb = {
          safeGetRandElemOf(bound) match {
            case Some(value) => Symb(value)
            case None => Symb(getRandElemOf(vars))
          }
        }
      };

      def predBuilder(em: ExprFactory): Predicate = {
        current_var = (current_var + 1) % num_preds
        val (n, arity) = preds(current_var)
        Predicate(n, List.fill[Symb](arity)(em.sym_builder()))
      }

      private val builders = Array(
        // nonterminals
        (em: ExprFactory, depth: Int) =>
          em.add_binder() match {
            case Some(sym) => ForAll(sym, em.make(depth - 1))
            case None => predBuilder(em)
          },
        (em: ExprFactory, depth: Int) =>
          em.add_binder() match {
            case Some(sym) => Exist(sym, em.make(depth - 1))
            case None => predBuilder(em)
          },
        (em: ExprFactory, depth: Int) =>
          App2(binOps(Random.between(0, binOps.length)), em.make(depth - 1), em.make(depth - 1)),
        (em: ExprFactory, depth: Int) => {
          App1(Neg, em.make(depth - 1))
        },
      )

      def run(): Formula = {
        val em = new ExprFactory
        em.make(depth)
      }
    }
    assert(num_preds > 0)
    assert(num_vars > 0)
    assert(depth > 0)
    FormulaMaker.run()
  }
}