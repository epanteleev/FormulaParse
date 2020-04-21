import java.io.PrintWriter

import Compiler.Compile
import IR.{Convert, Parse}

object Main {
    def main(args: Array[String]): Unit = {

        val program =
            """
              |val y = 5
              |y
              |""".stripMargin

        val a = Convert(Parse(program))
        new PrintWriter("code.ir") {
            write(a.toString)
            close()
        }

        val asm = Compile(a)
        new PrintWriter("out.asm") {
            write(asm.toString)
            close()
        }
    }
}