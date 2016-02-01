import scalaz._, Scalaz._

object EchozEchoz { 

  /* Impure program */

  def echo: String = {
    val msg = readLine
    println(msg)
    msg
  }

  /* Functional solution */

  object Fun {

    // Effect language

    type IOProgram[A] = Free[IOEffect, A]

    object IOProgram {
      def write(msg: String): IOProgram[Unit] = Free.liftF(Write(msg))
      def read: IOProgram[String] = Free.liftF(Read)
    }

    sealed trait IOEffect[A]
    case class Write(msg: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program

    import IOProgram._

    def pureEcho: IOProgram[String] =
      for {
        s <- read
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

    def echo: String = pureEcho.foldMap(consoleInterpreter)

  }

  
}
