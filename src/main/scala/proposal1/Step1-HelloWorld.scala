
object HelloWorld{ 

  /* Impure program */

  def hello(): Unit =
    println("Hello, world!")

  /* Functional solution */

  object Fun{

    // Effect language

    type IOProgram = Write

    case class Write(msg: String)

    // Program

    def pureHello(): IOProgram = 
      Write("Hello, world!")

    // Interpreter

    def run(program: IOProgram): Unit = 
      program match {
        case Write(msg) => println(msg)
      }

    // Composition

    def hello() = run(pureHello())

  }

  
}

