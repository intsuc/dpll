package dpll

enum Formula:
  case Var(x1: Symbol)
  case Not(f1: Formula)
  case And(f1: Formula, f2: Formula)
  case Or(f1: Formula, f2: Formula)

  def unary_! = Not(this)

  def &&(f2: Formula) = And(this, f2)

  def ||(f2: Formula) = Or(this, f2)

  override def toString: String = this match
    case Var(x1)     => s"$x1"
    case Not(f1)     => s"¬$f1"
    case And(f1, f2) => s"($f1 ∧ $f2)"
    case Or(f1, f2)  => s"($f1 ∨ $f2)"

extension (x1: Symbol) def unary_~ = Formula.Var(x1)

extension (x1: String) def unary_~ = Formula.Var(Symbol.fresh(x1))
