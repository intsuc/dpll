package dpll

enum LabeledFormula:
  case Var(label: Symbol)
  case Not(label: Symbol, f1: LabeledFormula)
  case And(label: Symbol, f1: LabeledFormula, f2: LabeledFormula)
  case Or(label: Symbol, f1: LabeledFormula, f2: LabeledFormula)
  val label: Symbol

  override def toString: String = this match
    case Var(label)         => s"$label"
    case Not(label, f1)     => s"(¬${f1})_$label"
    case And(label, f1, f2) => s"($f1 ∧ $f2)_$label"
    case Or(label, f1, f2)  => s"($f1 ∨ $f2)_$label"
