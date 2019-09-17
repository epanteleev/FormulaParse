
import Calculator.MakeByteCode
import org.scalatest.{FlatSpec, Matchers}
import Compiler.Compilate

class TestСompiler extends FlatSpec with Matchers {

  trait TestCase {

    val validCode: String = """x = 2+3\n x"""

    val asmCode: String =
      """	.global main
      |	.text
      |main:
      |	movl	$10, %ebx
      |	movl	$1, %ecx
      |	movl	%ebx, %eax
      |	imull	%ecx, %ebx
      |	pushl	%ebx
      |	movl	$8, %ebx
      |	movl	%ebx, 0(%esp)
      |	movl	$4, %ebx
      |	pushl	%ebx
      |	jmp	L3
      |L2:
      |	jmp	L1
      |L0:
      |	movl	0(%esp), %ebx
      |	movl	$1, %ecx
      |	movl	%ebx, %eax
      |	addl	%ecx, %ebx
      |	movl	%ebx, 0(%esp)
      |L1:
      |	cmpl $100, 0(%esp)
      |	jne	L0
      |	jmp	L4
      |L3:
      |	cmpl $4, 0(%esp)
      |	je	L2
      |L4:
      |	movl	$9, %ebx
      |	pushl	%ebx
      |	jmp	L6
      |L5:
      |	movl	0(%esp), %ebx
      |	movl	8(%esp), %ecx
      |	movl	%ebx, %eax
      |	addl	%ecx, %ebx
      |	movl	%ebx, 4(%esp)
      |	jmp	L7
      |L6:
      |	cmpl $9, 0(%esp)
      |	je	L5
      |L7:
      |	movl	4(%esp), %ebx
      |	movl	%ebx, %eax
      |	addl	$12, %esp
      |	ret""".stripMargin

  }


  val programm: String =
    """
      |y = 10 * 1
      |y = 8
      |x = 4
      |if( x == 4){
      |  while ( x != 100){
      |   x = x + 1
      |  }
      |}
      |t = 9
      |if(t == 9){
      | x = t + y
      |}
      |return x
      |""".stripMargin

// new TestCase {
//   "Test" should "successfully" in {
//     Compilate(programm) shouldBe asmCode
//   }
//
// }

}