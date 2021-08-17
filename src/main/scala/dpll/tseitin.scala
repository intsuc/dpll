package dpll

def tseitin(formula: Formula): Formula =
  def labeled: Formula => LabeledFormula =
    case Formula.Var(label)  => LabeledFormula.Var(label)
    case Formula.Not(f1)     => LabeledFormula.Not(Symbol.fresh(), labeled(f1))
    case Formula.And(f1, f2) => LabeledFormula.And(Symbol.fresh(), labeled(f1), labeled(f2))
    case Formula.Or(f1, f2)  => LabeledFormula.Or(Symbol.fresh(), labeled(f1), labeled(f2))

  def disjunctions: LabeledFormula => Seq[Formula] =
    case LabeledFormula.Var(label) =>
      Seq.empty
    case LabeledFormula.Not(label, f1) =>
      Seq(
        $(label) ∨ $(f1.label),
        ¬($(label)) ∨ ¬($(f1.label))
      )
        ++ disjunctions(f1)
    case LabeledFormula.And(label, f1, f2) =>
      Seq(
        ¬($(label)) ∨ $(f1.label),
        ¬($(label)) ∨ $(f2.label),
        $(label) ∨ ¬($(f1.label)) ∨ ¬($(f2.label))
      )
        ++ disjunctions(f1)
        ++ disjunctions(f2)
    case LabeledFormula.Or(label, f1, f2) =>
      Seq(
        $(label) ∨ ¬($(f1.label)),
        $(label) ∨ ¬($(f2.label)),
        ¬($(label)) ∨ $(f1.label) ∨ $(f2.label)
      )
        ++ disjunctions(f1)
        ++ disjunctions(f2)

  val root = labeled(formula)
  disjunctions(root).foldLeft($(root.label))(Formula.And(_, _))
