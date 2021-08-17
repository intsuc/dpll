package dpll

final case class Symbol private (name: String, id: Int):
  override def toString: String = s"$name$id"

object Symbol:
  import java.util.concurrent.atomic.AtomicInteger

  private val id: AtomicInteger = AtomicInteger(0)

  def fresh(name: String): Symbol = Symbol(name, id.getAndIncrement())

  def fresh(): Symbol = Symbol.fresh("")
