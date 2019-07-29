package exoskeleton

import scala.util._

trait Case[Cond, Value, Then]

object Parser {
  delegate for (Parser[Int] { type F = Try }) = xs => Try(xs.get.head.toInt)
  delegate for (Parser[String] { type F[T] = T }) = _.get.head
}

trait Parser[T] {
  type F[_]
  def parse(xs: Option[List[String]]): F[T]
}

sealed trait Part[Value, F[_]] {
  def apply() given Context[_ <: Case[this.type, _, _]] = from(the[Context[_ <: Case[this.type, _, _]]].invocation)
  def from(invocation: Invocation): F[Value]
}

object Arg {
  def apply[Value](name: String & Singleton) given (parser: Parser[Value]): Arg[name.type, Value, parser.F] = Arg(name)
}

class Arg[Name <: String & Singleton, Value, Wrap[_]](name: Name) given (val parser: Parser[Value] { type F = Wrap }) extends Part[Value, Wrap] {
  def from(invocation: Invocation): parser.F[Value] = parser.parse(invocation.map.get(name))
}

object Item {
  def apply[Value](index: Int & Singleton) given (parser: Parser[Value]): Item[index.type, Value, parser.F] = Item(index)
}

class Item[Index <: Int & Singleton, Value, Wrap[_]](index: Int) given (val parser: Parser[Value] { type F = Wrap }) extends Part[Value, Wrap] {
  def from(invocation: Invocation): parser.F[Value] = parser.parse(Some(List(invocation.prefix(index))))
}

case class Invocation(prefix: List[String], map: Map[String, List[String]], suffix: List[String] = Nil) {
  def get[N <: String & Singleton, Value, F[_]](arg: Arg[N, Value, F]): F[Value] = arg.from(this)
}

case class Context[T](invocation: Invocation) {
  def when[N <: String & Singleton, V, S, F[_]](arg: Arg[N, V, F], value: V) given (T <:< Case[arg.type, value.type, S]): WithContext[S & T] = WithContext(Context(Invocation(List(), Map())))
  def get[N <: String & Singleton, V, F[_]](arg: Arg[N, V, F]) given (T <:< arg.type): F[V] = invocation.get(arg)
}

def on[N <: String & Singleton, T, V, S, F[_]](arg: Arg[N, V, F], value: V) given (ctx: Context[T], ev: T <:< Case[arg.type, value.type, S]): WithContext[S & T] = WithContext(Context(Invocation(List(), Map())))

case class WithContext[T](ctx: Context[T]) {
  def apply[R](action: given Context[T] => R): R = action given ctx
}

object Foo {
  val alpha = Arg[Int]("alpha")
  val beta = Arg[String]("beta")
  val gamma = Arg[Int]("gamma")
  val delta = Arg[String]("delta")

  val item1: Item[0, String, [T] =>> T] = Item[String](0)
  val item2: Item[0, Int, Try] = Item[Int](0)
}

object Foo2 {
  import Foo._

  delegate for Context[Case[item1.type, _, Any] & Case[alpha.type, _, Any] & Case[beta.type, "one", Case[gamma.type, _, Any] & Case[delta.type, _, Any]]](Invocation(List("item1"), Map("alpha" -> List("one"), "beta" -> List("2"))))

  println(alpha())
  //println(gamma())

  val x: Try[Int] = on(beta, "one") {
    gamma()
  }

  on(beta, "one") {
    delta()
  }
}
