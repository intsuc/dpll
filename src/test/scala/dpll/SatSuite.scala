package dpll

import munit.FunSuite

class SatSuite extends FunSuite:
  def sat(formula: Formula): Option[Sat.Assignment] = Sat(Tseitin(formula))

  test("trivial") {
    val a = ~"a"
    assert(sat(a).isDefined)
  }

  test("trivial-contradiction") {
    val a = ~"a"
    val f = a && !a
    assert(sat(f).isEmpty)
  }

  test("trivial-and-idempotent") {
    val a = ~"a"
    val f = a && a
    assert(sat(f).isDefined)
  }

  test("trivial-or-idempotent") {
    val a = ~"a"
    val f = a || a
    assert(sat(f).isDefined)
  }

  test("trivial-and") {
    val a = ~"a"
    val b = ~"b"
    val f = a && b
    assert(sat(a).isDefined)
  }

  test("trivial-or") {
    val a = ~"a"
    val b = ~"b"
    val f = a || b
    assert(sat(a).isDefined)
  }
