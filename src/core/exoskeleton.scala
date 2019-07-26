package exoskeleton

case class Arg[T](name: String, short: Option[Char] = None)

case class Suggestion(completion: String, description: String)

case class CliInvocation(args: Seq[String], env: Map[String, String])

case class CommandState[ArgType]()

case class Context() {
}

trait Completable[S] { def completions: Iterable[Suggestion] }

trait Language[F[_]] {

  def empty: CommandState[Arg[_]] = CommandState[Any]()

  def hint[A, T, S: Completable]
          (oldState: State[A], arg: Arg[T], completions: Iterable[S])
          : F[A with arg.Type]

  def check[T](state: F[A], arg: Arg[T]): Option[T]
  def get[T](arg: Arg[T]): T
  def exec(args: CliInvocation)(fn: Context => ExitStatus): ExitStatus
}

object Run extends Language[???] {
  def empty
}

object ZshSuggest extends Language[???] {
}

object BashSuggest extends Language[???] {
  
}

object Example {
  val alpha = Arg[String]("alpha", short = Some('a'))
  val beta  = Arg[Int]("beta",  short = Some('b'))
  val gamma = Arg[Double]("gamma", short = Some('c'))


}