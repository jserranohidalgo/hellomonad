
object EchoEchoReloaded { 

  /* Impure program */

  def echo: String = {
    val msg = readLine
    println(msg)
    msg
  }

  /* Functional solution */

  object Fun {

    // Effect language

    sealed trait IOProgram[A]
    case class Effect[A](inst: IOEffect[A]) extends IOProgram[A]
    case class Sequence[A, B](p: IOProgram[A], cont: A => IOProgram[B]) extends IOProgram[B]
    case class Value[A](a: A) extends IOProgram[A]

    sealed trait IOEffect[A]
    case class Write(msg: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program

    def pureEcho: IOProgram[String] =
      Sequence(Effect(Read), (s: String) =>
        Sequence(Effect(Write(s)), (_: Unit) =>
          Value(s)
        )
      )

    // Interpreter

    def runProgram[A](program: IOProgram[A]): A =
      program match {
        case Effect(inst) => runEffect(inst)
        case Sequence(p, cont) =>
          val res = runProgram(p)
          val p2 = cont(res)
          runProgram(p2)
        case Value(a) => a
      }

    def runEffect[A](inst: IOEffect[A]): A =
      inst match {
        case Write(msg) => println(msg)
        case Read => readLine
      }

    // Composition

    def echo: String = runProgram(pureEcho)

  }

  
}
