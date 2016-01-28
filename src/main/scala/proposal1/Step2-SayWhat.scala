
object SayWhat { 

  /* Impure program */

  def hello: Unit =
    println("Hello, world!")

  def sayWhat: String =
    readLine

  /* Functional solution */

  object Fun {

    // Effect language

    type IOProgram[A] = IOEffect[A]

    sealed trait IOEffect[A]
    case class Write(msg: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program

    def pureHello: IOProgram[Unit] =
      Write("Hello, world!")

    def pureSayWhat: IOProgram[String] =
      Read

    // Interpreter

    def run[A](program: IOProgram[A]): A =
      program match {
        case Write(msg) => println(msg)
        case Read => readLine
      }

    // Composition

    def hello: Unit = run(pureHello)
    def sayWhat: String = run(pureSayWhat)

  }

  
}

