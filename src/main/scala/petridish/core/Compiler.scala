/*
  Copyright (c) 2016, Elliot Stirling
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

  * Neither the name of the {organization} nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package petridish.core

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes.{U1, U2}
import cafebabe.{CafebabeClassLoader, ClassFile, DynObj}
import evolve.core.{Instruction, Program}

import scala.annotation.tailrec

/**
  * For turning a program into a 'compiled' program
  */
object Compiler {

  def apply[A](program: Program)(implicit functions: Seq[Function[A]]): DynObj = {
    val cf = classFile(program, "Program")
    val cl = new CafebabeClassLoader
    cl.register(cf)
    cl.newInstance("Program")
  }

  def classFile[A](program: Program, name: String, parentName: Option[String] = None)(implicit functions: Seq[Function[A]]): ClassFile = {
    val usage = usage_count(program)

    val bytecode = functions.head.typ match {
      case "D" => compile64(program, usage)
      case "F" => compile32(program, usage)
      case "I" => compile32(program, usage)
      case "Z" => compile32(program, usage)
    }

    assert(usage.forall(_ == 0))
    val cf = new ClassFile(name, None)
    cf.addDefaultConstructor()
    val m1 = cf.addMethod("[" + functions.head.typ, "run", List.fill(program.inputCount)(functions.head.typ))
    val ch1 = m1.codeHandler

    (0 until bytecode._2 - program.inputCount).foreach(_ => ch1.getFreshVar(functions.head.typ))
    bytecode._1.foreach(bc => ch1 << bc)
    ch1 << Ldc(program.outputCount)
    ch1 << (functions.head.typ match {
      case "D" => NewArray.primitive("T_DOUBLE")
      case "F" => NewArray.primitive("T_FLOAT")
      case "I" => NewArray.primitive("T_INT")
      case "S" => NewArray.primitive("T_SHORT")
      case "B" => NewArray.primitive("T_BYTE")
      case "Z" => NewArray.primitive("T_BOOLEAN")
    })
    ch1 << ASTORE_1
    val storeCode = functions.head.typ match {
      case "D" => DASTORE
      case "F" => FASTORE
      case "I" => IASTORE
      case "S" => SASTORE
      case "B" => BASTORE
      case "Z" => BASTORE
    }
    functions.head.typ match {
      case "Z" | "B" | "S" | "I" | "F" =>
        (program.outputCount - 1 to 0 by -1).foreach { index =>
          ch1 << ALOAD_1 << SWAP
          ch1 << Ldc(index) << SWAP
          ch1 << storeCode
        }
      case "D" =>
        (program.outputCount - 1 to 0 by -1).foreach { index =>
          ch1 << ALOAD_1 << DUP_X2 << POP
          ch1 << Ldc(index) << DUP_X2 << POP
          ch1 << storeCode
        }
    }

    ch1 << ALOAD_1
    ch1 << ARETURN
    ch1.freeze()

    cf
  }

  /**
    * Calculates the usage count of each of the instructions in a program.
    * Used in compilation
    */
  private def usage_count(program: Program)(implicit functions: Seq[Function[_]]): Array[Int] = {
    val used: Array[Int] = Array.fill(program.data.length + program.inputCount)(0)

    for (i <- used.length - program.outputCount until used.length) {
      used(i) = 1
    }

    for {
      (inst, index) <- program.data.zipWithIndex.reverse
      func = functions(inst.instruction(program.instructionSize))
      input <- (0 until func.arguments)
      pointer = inst.pointer(program.instructionSize + (func.argumentSize * input), func.argumentSize)
    } {
      assert(func.arguments > 0)
      if (used(index + program.inputCount) > 0) {
        used(pointer) += 1
      }
    }

    used
  }

  private def inputCount[A](index: Int, program: Program)(implicit functions: Seq[Function[A]]): Int = if (index >= program.inputCount) {
    val inst: Instruction = program.data(index - program.inputCount)
    val func: Function[_] = functions(inst.instruction(program.instructionSize))
    func.arguments
  } else 0

  private def inputs[A](index: Int, program: Program)(implicit functions: Seq[Function[A]]): List[Int] = if (index >= program.inputCount) {
    val inst: Instruction = program.data(index - program.inputCount)
    val func: Function[_] = functions(inst.instruction(program.instructionSize))

    @tailrec def inputs(arguments: Int, acc: List[Int]): List[Int] = if (arguments > 0) {
      inputs(arguments - 1, inst.pointer(func.instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize) :: acc)
    } else acc.reverse

    inputs(func.arguments, Nil)
  } else Nil

  private def getField[A](field: Int)(implicit functions: Seq[Function[A]]): List[AbstractByteCode] = {
    functions.head.typ match {
      case "B" | "S" | "I" | "Z" =>
        field match {
          case 0 => List[AbstractByteCode](ILOAD_0)
          case 1 => List[AbstractByteCode](ILOAD_1)
          case 2 => List[AbstractByteCode](ILOAD_2)
          case 3 => List[AbstractByteCode](ILOAD_3)
          case _ if field >= 0 && field <= 127 => List[AbstractByteCode](ILOAD, RawByte(field.asInstanceOf[U1]))
          case _ if field >= 0 && field <= 32767 => List[AbstractByteCode](WIDE, ILOAD, RawBytes(field.asInstanceOf[U2]))
          case _ => sys.error("Invalid field " + field)
        }

      case "F" =>
        field match {
          case 0 => List[AbstractByteCode](FLOAD_0)
          case 1 => List[AbstractByteCode](FLOAD_1)
          case 2 => List[AbstractByteCode](FLOAD_2)
          case 3 => List[AbstractByteCode](FLOAD_3)
          case _ if field >= 0 && field <= 127 => List[AbstractByteCode](FLOAD, RawByte(field.asInstanceOf[U1]))
          case _ if field >= 0 && field <= 32767 => List[AbstractByteCode](WIDE, FLOAD, RawBytes(field.asInstanceOf[U2]))
          case _ => sys.error("Invalid field " + field)
        }

      case "D" =>
        val register = (field * 2) - 1
        register match {
          case 0 => List[AbstractByteCode](DLOAD_0)
          case 1 => List[AbstractByteCode](DLOAD_1)
          case 2 => List[AbstractByteCode](DLOAD_2)
          case 3 => List[AbstractByteCode](DLOAD_3)
          case _ if register >= 0 && register <= 127 => List[AbstractByteCode](DLOAD, RawByte(register.asInstanceOf[U1]))
          case _ if register >= 0 && register <= 32767 => List[AbstractByteCode](WIDE, DLOAD, RawBytes(register.asInstanceOf[U2]))
          case _ => sys.error("Invalid field in " + register)
        }
    }
  }

  private def setField[A](field: Int)(implicit functions: Seq[Function[A]]): List[AbstractByteCode] = {
    functions.head.typ match {
      case "B" | "S" | "I" | "Z" =>
        field match {
          case 0 => List[AbstractByteCode](ISTORE_0)
          case 1 => List[AbstractByteCode](ISTORE_1)
          case 2 => List[AbstractByteCode](ISTORE_2)
          case 3 => List[AbstractByteCode](ISTORE_3)
          case _ if field >= 0 && field <= 127 => List[AbstractByteCode](ISTORE, RawByte(field.asInstanceOf[U1]))
          case _ if field >= 0 && field <= 32767 => List[AbstractByteCode](WIDE, ISTORE, RawBytes(field.asInstanceOf[U2]))
          case _ => sys.error("Invalid field " + field)
        }
      case "F" =>
        field match {
          case 0 => List[AbstractByteCode](FSTORE_0)
          case 1 => List[AbstractByteCode](FSTORE_1)
          case 2 => List[AbstractByteCode](FSTORE_2)
          case 3 => List[AbstractByteCode](FSTORE_3)
          case _ if field >= 0 && field <= 127 => List[AbstractByteCode](FSTORE, RawByte(field.asInstanceOf[U1]))
          case _ if field >= 0 && field <= 32767 => List[AbstractByteCode](WIDE, FSTORE, RawBytes(field.asInstanceOf[U2]))
          case _ => sys.error("Invalid field " + field)
        }
      case "D" =>
        val register = (field * 2) - 1
        register match {
          case 0 => List[AbstractByteCode](DSTORE_0)
          case 1 => List[AbstractByteCode](DSTORE_1)
          case 2 => List[AbstractByteCode](DSTORE_2)
          case 3 => List[AbstractByteCode](DSTORE_3)
          case _ if register >= 0 && register <= 127 => List[AbstractByteCode](DSTORE, RawByte(register.asInstanceOf[U1]))
          case _ if register >= 0 && register <= 32767 => List[AbstractByteCode](WIDE, DSTORE, RawBytes(register.asInstanceOf[U2]))
          case _ => sys.error("Invalid field " + register)
        }
    }
  }

  private def getByteCode[A](index: Int, program: Program)(implicit functions: Seq[Function[A]]): List[AbstractByteCode] = {
    if (index < program.inputCount) {
      val load = index + 1
      getField(load)
    } else {
      val inst: Instruction = program.data(index - program.inputCount)
      val func: Function[_] = functions(inst.instruction(program.instructionSize))
      func.compile(inst)
    }
  }

  private def compile32[A](program: Program, usage: Array[Int])(implicit functions: Seq[Function[A]]): (List[AbstractByteCode], Int) = {

    val outputs = (0 until program.outputCount).map(a => program.data.length + program.inputCount - a - 1).toList

    def orderInputs(inputs: List[Int]): (List[Int], List[ByteCode]) = {
      def process(stack: List[Int], code: List[ByteCode]): (List[Int], List[ByteCode]) = if (stack.sorted.reverse != stack) {
        stack match {
          case head1 :: head2 :: Nil =>
            process(head2 :: head1 :: Nil, SWAP :: code)
          case _ => throw new RuntimeException("unhandled to sort inputs")
        }
      } else (stack, code)
      process(inputs, Nil)
    }

    @tailrec def process(stack: List[Int], fields: Seq[Int], code: List[AbstractByteCode]): (List[AbstractByteCode], Int) = {
      stack match {

        /*
         * only one output and input. so just generate
         */
        case head :: tail if usage(head) == 1 && inputCount(head, program) == 1 && !(fields contains head) =>
          usage(head) -= 1
          process(inputs(head, program) ::: tail, fields, getByteCode(head, program) ::: code)

        /*
         * Remove pairs of pairs of values in the stack
         */

        case head1 :: head2 :: head3 :: head4 :: tail if head1 == head3 && head2 == head4 =>
          usage(head1) -= 1
          usage(head2) -= 1
          process(head1 :: head2 :: tail, fields, DUP2 :: code)

        case head1 :: head2 :: head3 :: head4 :: head5 :: tail if head1 == head4 && head2 == head5 =>
          usage(head1) -= 1
          usage(head2) -= 1
          process(head1 :: head2 :: head3 :: tail, fields, DUP2_X1 :: code)

        case head1 :: head2 :: head3 :: head4 :: head5 :: head6 :: tail if head1 == head5 && head2 == head6 =>
          usage(head1) -= 1
          usage(head2) -= 1
          process(head1 :: head2 :: head3 :: head4 :: tail, fields, DUP2_X2 :: code)

        case head1 :: head2 :: head3 :: head4 :: tail if head1 == head4 && head2 == head3 =>
          usage(head1) -= 1
          usage(head2) -= 1
          process(head2 :: head1 :: tail, fields, DUP2 :: SWAP :: code)

        case head1 :: head2 :: head3 :: head4 :: head5 :: tail if head1 == head5 && head2 == head4 =>
          usage(head1) -= 1
          usage(head2) -= 1
          process(head2 :: head1 :: head3 :: tail, fields, DUP2_X1 :: SWAP :: code)

        case head1 :: head2 :: head3 :: head4 :: head5 :: head6 :: tail if head1 == head6 && head2 == head5 =>
          usage(head1) -= 1
          usage(head2) -= 1
          process(head2 :: head1 :: head3 :: head4 :: tail, fields, DUP2_X2 :: SWAP :: code)

        /*
         * Remove pairs of values from the stack
         */

        case head1 :: head2 :: tail if head1 == head2 =>
          usage(head1) -= 1
          process(head1 :: tail, fields, DUP :: code)

        case head1 :: head2 :: head3 :: tail if head1 == head3 =>
          usage(head1) -= 1
          process(head1 :: head2 :: tail, fields, DUP_X1 :: code)

        case head1 :: head2 :: head3 :: head4 :: tail if head1 == head4 =>
          usage(head1) -= 1
          process(head1 :: head2 :: head3 :: tail, fields, DUP_X2 :: code)

        case head1 :: head2 :: head3 :: tail if head2 == head3 =>
          usage(head2) -= 1
          process(head2 :: head1 :: tail, fields, DUP_X1 :: SWAP :: code)

        case head1 :: head2 :: head3 :: head4 :: tail if head2 == head4 =>
          usage(head2) -= 1
          process(head2 :: head1 :: head3 :: tail, fields, DUP_X2 :: SWAP :: code)

        case head1 :: head2 :: head3 :: head4 :: tail if head3 == head4 =>
          usage(head3) -= 1
          process(head3 :: head1 :: head2 :: tail, fields, DUP :: DUP2_X2 :: POP2 :: code)

        /*
         * Field Stores
         */
        case head :: tail if (fields contains head) && usage(head) == 1 && fields.indexOf(head) > program.inputCount =>
          usage(head) -= 1
          val input = orderInputs(inputs(head, program))
          process(input._1 ::: tail, fields.updated(fields.indexOf(head), -2), input._2 ::: getByteCode(head, program) ::: DUP :: setField(fields.indexOf(head)) ::: code)

        case head1 :: head2 :: tail if (fields contains head2) && usage(head2) == 1 && fields.indexOf(head2) > program.inputCount =>
          usage(head2) -= 1
          val input = orderInputs(inputs(head2, program))
          process(input._1 ::: head1 :: tail, fields.updated(fields.indexOf(head2), -2), input._2 ::: getByteCode(head2, program) ::: DUP :: setField(fields.indexOf(head2)) ::: SWAP :: code)

        case head1 :: head2 :: head3 :: tail if (fields contains head3) && usage(head3) == 1 && fields.indexOf(head3) > program.inputCount =>
          usage(head3) -= 1
          val input = orderInputs(inputs(head3, program))
          process(input._1 ::: head1 :: head2 :: tail, fields.updated(fields.indexOf(head3), -2), input._2 ::: getByteCode(head3, program) ::: DUP :: setField(fields.indexOf(head3)) ::: DUP_X2 :: POP :: code)

        /*
         * Field Loads
         */
        case head :: tail if fields contains head =>
          usage(head) -= 1
          process(tail, fields, getField(fields.indexOf(head)) ::: code)

        case head1 :: head2 :: tail if fields contains head2 =>
          usage(head2) -= 1
          process(head1 :: tail, fields, getField(fields.indexOf(head2)) ::: SWAP :: code)

        case head1 :: head2 :: head3 :: tail if fields contains head3 =>
          usage(head3) -= 1
          process(head1 :: head2 :: tail, fields, getField(fields.indexOf(head3)) ::: DUP_X2 :: POP :: code)

        /*
         * Local variable registration
         */
        case head :: tail if usage(head) > 1 =>
          usage(head) -= 1
          if (fields.contains(-2)) {
            process(tail, fields.updated(fields.indexOf(-2), head), getField(fields.indexOf(-2)) ::: code)
          } else {
            process(tail, fields :+ head, getField(fields.length) ::: code)
          }

        /*
         * Actually compile the program section when it is required
         */

        case head :: tail =>
          usage(head) -= 1
          val input = orderInputs(inputs(head, program))
          process(input._1 ::: tail, fields, input._2 ::: getByteCode(head, program) ::: code)

        /*
         * Finished! Optimise here
         */
        case Nil =>
          (code, fields.length)
      }
    }

    process(outputs, -1 until program.inputCount, Nil)
  }

  private def compile64[A](program: Program, usage: Array[Int])(implicit functions: Seq[Function[A]]): (List[AbstractByteCode], Int) = {

    val outputs = (0 until program.outputCount).map(a => program.data.length + program.inputCount - a - 1).toList

    def orderInputs(inputs: List[Int]): (List[Int], List[ByteCode]) = {
      def process(stack: List[Int], code: List[ByteCode]): (List[Int], List[ByteCode]) = if (stack.sorted.reverse != stack) {
        stack match {
          case head1 :: head2 :: Nil =>
            process(head2 :: head1 :: Nil, DUP2_X2 :: POP2 :: code)
          case _ => throw new RuntimeException("unhandled to sort inputs")
        }
      } else (stack, code)
      process(inputs, Nil)
    }

    @tailrec def process(stack: List[Int], fields: Seq[Int], code: List[AbstractByteCode]): (List[AbstractByteCode], Int) = {
      stack match {

        /*
         * only one output and input. so just generate
         */
        case head :: tail if usage(head) == 1 && inputCount(head, program) == 1 && !(fields contains head) =>
          usage(head) -= 1
          process(inputs(head, program) ::: tail, fields, getByteCode(head, program) ::: code)

        /*
         * Remove pairs of values from the stack
         */

        case head1 :: head2 :: tail if head1 == head2 =>
          usage(head1) -= 1
          process(head1 :: tail, fields, DUP2 :: code)

        case head1 :: head2 :: head3 :: tail if head1 == head3 =>
          usage(head1) -= 1
          process(head1 :: head2 :: tail, fields, DUP2_X2 :: code)

        case head1 :: head2 :: head3 :: tail if head2 == head3 =>
          usage(head2) -= 1
          process(head2 :: head1 :: tail, fields, DUP2_X2 :: DUP2_X2 :: POP2 :: code)

        /*
         * Field Stores
         */
        case head :: tail if (fields contains head) && usage(head) == 1 && fields.indexOf(head) > program.inputCount =>
          usage(head) -= 1
          val input = orderInputs(inputs(head, program))
          process(input._1 ::: tail, fields.updated(fields.indexOf(head), -2), input._2 ::: getByteCode(head, program) ::: DUP2 :: setField(fields.indexOf(head)) ::: code)

        case head1 :: head2 :: tail if (fields contains head2) && usage(head2) == 1 && fields.indexOf(head2) > program.inputCount =>
          usage(head2) -= 1
          val input = orderInputs(inputs(head2, program))
          process(input._1 ::: head1 :: tail, fields.updated(fields.indexOf(head2), -2), input._2 ::: getByteCode(head2, program) ::: DUP2 :: setField(fields.indexOf(head2)) ::: DUP2_X2 :: POP2 :: code)

        /*
         * Field Loads
         */
        case head :: tail if fields contains head =>
          usage(head) -= 1
          process(tail, fields, getField(fields.indexOf(head)) ::: code)

        case head1 :: head2 :: tail if fields contains head2 =>
          usage(head2) -= 1
          process(head1 :: tail, fields, getField(fields.indexOf(head2)) ::: DUP2_X2 :: POP2 :: code)

        /*
         * Local variable registration
         */
        case head :: tail if usage(head) > 1 =>
          usage(head) -= 1
          if (fields.contains(-2)) {
            process(tail, fields.updated(fields.indexOf(-2), head), getField(fields.indexOf(-2)) ::: code)
          } else {
            process(tail, fields :+ head, getField(fields.length) ::: code)
          }

        /*
         * Actually compile the program section when it is required
         */

        case head :: tail =>
          usage(head) -= 1
          val input = orderInputs(inputs(head, program))
          process(input._1 ::: tail, fields, input._2 ::: getByteCode(head, program) ::: code)

        /*
         * Finished! Optimise here
         */
        case Nil =>
          (code, fields.length)
      }
    }

    process(outputs, -1 until program.inputCount, Nil)
  }
}
