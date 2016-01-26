
object HelloSayWhat { 

  /* Impure program */

  def hello: Unit =
    println("Hello, world!")

  def sayWhat: String =
    readLine("Say what?")

  def helloSayWhat: String = {
    hello
    sayWhat
  }

  /* Functional solution */

  object Fun {

    // Effect language

    sealed trait IOProgram[A]
    case class Instruction[A](inst: IOInstruction[A]) extends IOProgram[A]
    case class Sequence[A, B](p1: IOProgram[A], p2: IOProgram[B]) extends IOProgram[B]

    sealed trait IOInstruction[A]
    case class Write(msg: String) extends IOInstruction[Unit]
    case class Read(msg: String) extends IOInstruction[String]

    // Program

    def pureHello: IOProgram[Unit] =
      Instruction(Write("Hello, world!"))

    def pureSayWhat: IOProgram[String] =
      Instruction(Read("Say what?"))

    def pureHelloSayWhat: IOProgram[String] =
      Sequence(pureHello, pureSayWhat)

    // Interpreter

    def runProgram[A](program: IOProgram[A]): A =
      program match {
        case Instruction(inst) => runInstruction(inst)
        case Sequence(p1, p2) =>
          runProgram(p1)
          runProgram(p2)
      }

    def runInstruction[A](inst: IOInstruction[A]): A =
      inst match {
        case Write(msg) => println(msg)
        case Read(msg) => readLine(msg)
      }

    // Composition

    def hello: Unit = runProgram(pureHello)
    def sayWhat: String = runProgram(pureSayWhat)
    def helloSayWhat: String = runProgram(pureHelloSayWhat)

  }

  
}

