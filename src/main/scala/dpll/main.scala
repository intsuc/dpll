package dpll

type Literal = (Symbol, Boolean)

type Clause = Seq[Literal]

object Tseitin:
  private def labeled: Formula => LabeledFormula =
    case Formula.Var(label)  => LabeledFormula.Var(label)
    case Formula.Not(f1)     => LabeledFormula.Not(Symbol.fresh(), labeled(f1))
    case Formula.And(f1, f2) => LabeledFormula.And(Symbol.fresh(), labeled(f1), labeled(f2))
    case Formula.Or(f1, f2)  => LabeledFormula.Or(Symbol.fresh(), labeled(f1), labeled(f2))

  private def disjunctions: LabeledFormula => Seq[Clause] =
    case LabeledFormula.Var(label) =>
      Seq.empty
    case LabeledFormula.Not(label, f1) =>
      Seq((label, true), (f1.label, true))
        +: Seq((label, false), (f1.label, false))
        +: disjunctions(f1)
    case LabeledFormula.And(label, f1, f2) =>
      Seq((label, false), (f1.label, true))
        +: Seq((label, false), (f2.label, true))
        +: Seq((label, true), (f1.label, false), (f2.label, false))
        +: (disjunctions(f1) ++ disjunctions(f2))
    case LabeledFormula.Or(label, f1, f2) =>
      Seq((label, true), (f1.label, false))
        +: Seq((label, true), (f2.label, false))
        +: Seq((label, false), (f1.label, true), (f2.label, true))
        +: (disjunctions(f1) ++ disjunctions(f2))

  def apply(formula: Formula): Seq[Clause] =
    val root = labeled(formula)
    Seq((root.label, true)) +: disjunctions(root)

object Sat:
  import scala.collection.mutable

  type Assignment = Map[Symbol, Boolean]

  private def eval(assignment: Assignment, clause: Clause): Option[Clause] =
    val result = mutable.Buffer.empty[Literal]
    for literal <- clause do
      literal match
        case (symbol, polarity) if assignment.contains(symbol) =>
          if assignment(symbol) == polarity then return None
        case literal => result += literal
    Some(result.toSeq)

  private def choose(assignment: Assignment, clauses: Seq[Clause]): Literal =
    clauses.flatten.find((symbol, _) => !assignment.contains(symbol)).get

  private def satisfiable(assignment: Assignment, clauses: Seq[Clause]): Option[Assignment] =
    // TODO: apply rules
    clauses.flatMap(eval(assignment, _)) match
      case clauses if clauses.isEmpty           => Some(assignment)
      case clauses if clauses.exists(_.isEmpty) => None
      case clauses =>
        val (symbol, _) = choose(assignment, clauses)
        satisfiable(assignment.updated(symbol, true), clauses)
          .orElse(satisfiable(assignment.updated(symbol, false), clauses))

  def apply(clauses: Seq[Clause]): Option[Assignment] =
    satisfiable(Map.empty, clauses)
