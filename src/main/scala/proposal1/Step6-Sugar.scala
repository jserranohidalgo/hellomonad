
object Sugar { 

  /* Impure program */

  def echo: String = {
    val msg = readLine
    println(msg)
    msg
  }

  /* Functional solution */

  object Fun {

    // Effect language

    sealed trait IOProgram[A] {
      def flatMap[B](f: A => IOProgram[B]): IOProgram[B] =
        Sequence(this, f)

      def map[B](f: A => B): IOProgram[B] =
        this.flatMap(a => Value(f(a)))

    }
    case class Effect[A](inst: IOEffect[A]) extends IOProgram[A]
    case class Sequence[A, B](p: IOProgram[A], cont: A => IOProgram[B]) extends IOProgram[B]
    case class Value[A](a: A) extends IOProgram[A]

    object IOProgram {
      def write(msg: String): IOProgram[Unit] =
        Effect(Write(msg))

      def read: IOProgram[String] =
        Effect(Read)
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
      } yield s

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
