
object EchoEcho { 

  /* Impure program */

  def echo: Unit = {
    val msg = readLine
    println(msg)
  }

  /* Functional solution */

  object Fun {

    // Effect language

    sealed trait IOProgram[A]
    case class Effect[A](inst: IOEffect[A]) extends IOProgram[A]
    case class Sequence[A, B](p: IOProgram[A], cont: A => IOProgram[B]) extends IOProgram[B]

    sealed trait IOEffect[A]
    case class Write(msg: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program

    def pureEcho: IOProgram[Unit] =
      Sequence(Effect(Read), (s: String) => 
        Effect(Write(s)))

    // Interpreter

    def runProgram[A](program: IOProgram[A]): A =
      program match {
        case Effect(inst) => runEffect(inst)
        case Sequence(p, cont) =>
          val res = runProgram(p)
          val p2 = cont(res)
          runProgram(p2)
      }

    def runEffect[A](inst: IOEffect[A]): A =
      inst match {
        case Write(msg) => println(msg)
        case Read => readLine
      }

    // Composition

    def echo: Unit = runProgram(pureEcho)

  }

  
}

