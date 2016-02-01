
object SayWhatReloaded { 

  /* Impure program */

  def sayWhat: String = {
    println("Say what?")
    readLine
  }

  /* Functional solution */

  object Fun {

    // Effect language

    sealed trait IOProgram[A]
    case class Effect[A](inst: IOEffect[A]) extends IOProgram[A]
    case class Sequence[A, B](p1: IOProgram[A], p2: IOProgram[B]) extends IOProgram[B]

    sealed trait IOEffect[A]
    case class Write(msg: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program

    def pureSayWhat: IOProgram[String] =
      Sequence(Effect(Write("Say what?")), 
        Effect(Read))

    // Interpreter

    def runProgram[A](program: IOProgram[A]): A =
      program match {
        case Effect(inst) => runEffect(inst)
        case Sequence(p1, p2) =>
          runProgram(p1)
          runProgram(p2)
      }

    def runEffect[A](inst: IOEffect[A]): A =
      inst match {
        case Write(msg) => println(msg)
        case Read => readLine
      }

    // Composition

    def helloSayWhat: String = runProgram(pureSayWhat)

  }

  
}

