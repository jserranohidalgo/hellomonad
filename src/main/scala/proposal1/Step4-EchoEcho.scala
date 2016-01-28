
object EchoEcho { 

  /* Impure program */

  def hello: Unit =
    println("Hello, world!")

  def sayWhat: String =
    readLine

  def helloSayWhat: String = {
    hello
    sayWhat
  }

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

    def pureHello: IOProgram[Unit] =
      Effect(Write("Hello, world!"))

    def pureSayWhat: IOProgram[String] =
      Effect(Read)

    def pureHelloSayWhat: IOProgram[String] =
      Sequence(pureHello, (_: Unit) => pureSayWhat)

    def pureEcho: IOProgram[Unit] =
      Sequence(pureSayWhat, (s: String) => Effect(Write(s)))

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

    def hello: Unit = runProgram(pureHello)
    def sayWhat: String = runProgram(pureSayWhat)
    def helloSayWhat: String = runProgram(pureHelloSayWhat)
    def echo: Unit = runProgram(pureEcho)

  }

  
}

