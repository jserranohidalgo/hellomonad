
object SayWhat { 

  /* Impure program */

  def hello: Unit =
    println("Hello, world!")

  def sayWhat: String =
    readLine("Say what?")

  /* Functional solution */

  object Fun {

    // Effect language

    type IOProgram[A] = IOInstruction[A]

    sealed trait IOInstruction[A]
    case class Write(msg: String) extends IOInstruction[Unit]
    case class Read(msg: String) extends IOInstruction[String]

    // Program

    def pureHello: IOProgram[Unit] =
      Write("Hello, world!")

    def pureSayWhat: IOProgram[String] =
      Read("Say what?")

    // Interpreter

    def run[A](program: IOProgram[A]): A =
      program match {
        case Write(msg) => println(msg)
        case Read(msg) => readLine(msg)
      }

    // Composition

    def hello: Unit = run(pureHello)
    def sayWhat: String = run(pureSayWhat)

  }

  
}

