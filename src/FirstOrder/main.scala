package FirstOrder

import fastparse.Parsed
import scopt.OParser

case class Config(
                   gen: Boolean = false,
                   normal_form: Option[String] = Option.empty[String],
                   compare: Option[(String, String)] = Option.empty[(String, String)],
                   is_normal: Option[String] = Option.empty[String]
                 )

object main {
  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Config]
    val parser1 = {
      import builder._
      OParser.sequence(
        programName("FirstOrder to Prenex"),
        head("Prenex", "0.1"),
        opt[Boolean]('g', "gen")
          .action((_, c) => c.copy(gen = true))
          .text("Generate a new formula and exit"),
        opt[String]("nf")
          .action((nf, c) => c.copy(normal_form = Some(nf)))
          .text("Print normal form"),
        opt[(String, String)]("compare")
          .action({ case ((l, r), c) => c.copy(compare = Some((l, r))) })
          .text("Compare formulas"),
        opt[String]("is_normal")
          .action((nf, c) => c.copy(normal_form = Some(nf)))
          .text("Check normal form"))
    }
    OParser.parse(parser1, args, Config()) match {
      case Some(config) => {
        if (config.gen)
          println(FirstOrder.Formula.randomFormula(3, 4, 4))

        if (config.normal_form.nonEmpty) {
          val parse_result = FormulaParser.Parse(config.normal_form.get)
          parse_result match {
            case Parsed.Success(value, _) => println("Prenex of ", config.normal_form.get, " is ",
              IndexedFormula.toPrenex(Formula.toIndexed(value)._1))
            case failure: Parsed.Failure =>
              println("Incorrect formula, error:\n", failure.trace())
              return
          }
        }
        if (config.compare.nonEmpty) {
          val (l, r) = (FormulaParser.Parse(config.compare.get._1), FormulaParser.Parse(config.compare.get._2))
          l match {
            case Parsed.Success(v1, _) =>
              r match {
                case Parsed.Success(v2, _) =>
                  val p1 = Formula.toIndexed(v1)._1
                  val p2 = Formula.toIndexed(v2)._1
                  println(config.compare.get._1, "==", config.compare.get._2, " is ", p1 == p2)
                case failure: Parsed.Failure =>
                  println("Incorrect formula, error:\n", failure.trace())
                  return
              }
            case failure: Parsed.Failure =>
              println("Incorrect formula, error:\n", failure.trace())
              return
          }
        }
        if (config.is_normal.nonEmpty) {
          val p = FormulaParser.Parse(config.is_normal.get)
          p match {
            case Parsed.Success(value, index) =>
              val indexed = Formula.toIndexed(value)._1
              val cond = indexed == IndexedFormula.toPrenex(indexed)
              println(config.is_normal.get, " is ", if (cond) "" else "not", " in normal form")
            case failure: Parsed.Failure =>
              println("Incorrect formula, error:\n", failure.trace())
              return
          }
        }
      }

      case _ =>
        print("Error occured!")
    }
  }
}