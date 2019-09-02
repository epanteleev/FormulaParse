
import org.scalatest.{FlatSpec, Matchers}
import Compilator.Compilate

class TestCompilator extends FlatSpec with Matchers {

  trait TestCase {

    val validCode: String = """x = 2+3\n x"""

    val asmCode: String =
      """         .data
        |x:      .int
        |
        |
        |        .global main
        |        .text
        |main:
        |        movl $2, %ecx
        |        movl $3,  %ebx
        |        addl %ecx, %ebx
        |        movl %ebx, x
        |	       movl x, %eax
        |        ret""".stripMargin

  }




  "Test" should "successfully" in {
    //e shouldBe a
  }



}