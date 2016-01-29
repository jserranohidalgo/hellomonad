import scalaz._, Scalaz._

object EchozEchoz { 

  /* Impure program */

  def hello: Unit =
    println("Hello, world!")

  def sayWhat: String =
    readLine

  def sayWhatReloaded: String = {
    hello
    sayWhat
  }

  def echo: Unit = {
    val msg = readLine
    println(msg)
  }

  def echoReloaded: String = {
    val msg = readLine
    println(msg)
    msg
  }

  /* Functional solution */

  object Fun {

    // Effect language

    sealed trait IOEffect[A]
    case class Write(msg: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    type IOProgram[A] = Free[IOEffect, A]

    object IOProgram {
      def write(msg: String): IOProgram[Unit] = Free.liftF(Write(msg))
      def read: IOProgram[String] = Free.liftF(Read)
    }

    // Program

    import IOProgram._

    def pureHello: IOProgram[Unit] =
      write("Hello, world!")

    def pureSayWhat: IOProgram[String] =
      read

    def pureSayWhatReloaded: IOProgram[String] =
      for {
        _ <- pureHello
        s <- pureSayWhat
      } yield s

    def pureEcho: IOProgram[Unit] =
      pureSayWhat flatMap write

    def pureEchoReloaded: IOProgram[String] =
      for {
        s <- pureSayWhat
        _ <- write(s)
      } yield (s)

    // Interpreter

    val consoleInterpreter = new ~>[IOEffect, Id] {
      def apply[A](eff: IOEffect[A]): A = eff match {
        case Write(msg) => println(msg)
        case Read => readLine
      }
      
    }

    // Composition

    def hello: Unit = pureHello.foldMap(consoleInterpreter)
    def sayWhat: String = pureSayWhat.foldMap(consoleInterpreter)
    def sayWhatReloaded: String = pureSayWhatReloaded.foldMap(consoleInterpreter)
    def echo: Unit = pureEcho.foldMap(consoleInterpreter)
    def echoReloaded: String = pureEchoReloaded.foldMap(consoleInterpreter)

  }

  
}
