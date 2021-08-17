package dpll

enum Formula:
  case Var(x1: Symbol)
  case Not(f1: Formula)
  case And(f1: Formula, f2: Formula)
  case Or(f1: Formula, f2: Formula)

  def ∧(f2: Formula) = And(this, f2)

  def ∨(f2: Formula) = Or(this, f2)

  override def toString: String = this match
    case Var(x1)     => s"$x1"
    case Not(f1)     => s"¬$f1"
    case And(f1, f2) => s"($f1 ∧ $f2)"
    case Or(f1, f2)  => s"($f1 ∨ $f2)"

import Formula.*

object $ :
  def apply(x1: String) = Var(Symbol.fresh(x1))

  def apply(x1: Symbol) = Var(x1)

object ¬ :
  def apply(f1: Formula) = Not(f1)
