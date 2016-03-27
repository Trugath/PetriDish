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

package petridish.functions

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.ClassFile
import cafebabe.ClassFileTypes.{U1, U2}
import evolve.core.{Instruction, Memory}
import petridish.core.Function

import scala.concurrent.forkjoin.ThreadLocalRandom

object IntegerFunctions {

  implicit val functions = Seq[Function[Int]](
    Nop,
    Const,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement,
    And, Or, XOr, Not,
    ShiftLeft, ShiftSignedRight, ShiftUnsignedRight
  )

  implicit def scoreFunc: (Option[Int], Option[Int]) => Long = (a, b) => {

    def nabs(i: Long): Long = if (i < 0) -i else i

    val result = (a, b) match {
      case (Some(left), Some(right)) => nabs(left - right)
      case (Some(left), None) => left.abs
      case (None, Some(right)) => right.abs
      case (None, None) => 0
    }
    assert(result >= 0)
    result * 10
  }

  object Nop extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Nop", "I").codeHandler
      ch1 << ILoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      Nil
    }

    override def arguments: Int = 1

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = "Nop"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a)
    }
  }

  object Const extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Const", "I").codeHandler
      ch1 << ILoad(1) << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      val i = inst.const(instructionSize, 32 - instructionSize)
      def const(value: Int): List[AbstractByteCode] = {
        value match {
          case -1 => List(ICONST_M1)
          case 0 => List(ICONST_0)
          case 1 => List(ICONST_1)
          case 2 => List(ICONST_2)
          case 3 => List(ICONST_3)
          case 4 => List(ICONST_4)
          case 5 => List(ICONST_5)
          case _ if value >= -128 && value <= 127 => List(BIPUSH, RawByte(value.asInstanceOf[U1]))
          case _ if value >= -32768 && value <= 32767 => List(SIPUSH, RawBytes(value.asInstanceOf[U2]))
          case _ => const(value & 0x7fff) ::: const(value >> 15) ::: const(15) ::: List(ISHL, IOR)
        }
      }

      const(i)
    }

    override def arguments: Int = 0

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize)
      s"Const ($value)"
    }

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      memory.append(inst.const(instructionSize, 32 - instructionSize))
    }
  }

  object Add extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Add", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IADD)
    }

    override def cost: Int = 4

    override def getLabel(inst: Instruction): String = "Add"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a + b)
    }
  }

  object Subtract extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Subtract", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(ISUB)
    }

    override def cost: Int = 4

    override def getLabel(inst: Instruction): String = "Subtract"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a - b)
    }
  }

  object Multiply extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Multiply", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IMUL)
    }

    override def cost: Int = 5

    override def getLabel(inst: Instruction): String = "Multiply"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a * b)
    }
  }

  object Divide extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Divide", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      val labeler: String = ThreadLocalRandom.current().nextInt().toString
      List(DUP, IfEq("Zero" + labeler), IDIV, Goto("End" + labeler), Label("Zero" + labeler), SWAP, POP, Label("End" + labeler))
    }

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Divide"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      try {
        memory.append(a / b)
      } catch {
        case e: ArithmeticException => memory.append(0)
      }
    }
  }

  object Modulus extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Modulus", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      val labeler: String = ThreadLocalRandom.current().nextInt().toString
      List(DUP, IfEq("Zero" + labeler), IREM, Goto("End" + labeler), Label("Zero" + labeler), SWAP, POP, Label("End" + labeler))
    }

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Modulus"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      try {
        memory.append(a % b)
      } catch {
        case e: ArithmeticException => memory.append(0)
      }
    }
  }

  object Increment extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Increment", "I").codeHandler
      ch1 << ILoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(ICONST_1, IADD)
    }

    override def arguments: Int = 1

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "Increment"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a + 1)
    }
  }

  object Decrement extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Decrement", "I").codeHandler
      ch1 << ILoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(ICONST_1, ISUB)
    }

    override def arguments: Int = 1

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "Decrement"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a - 1)
    }
  }

  object And extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "And", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IAND)
    }

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "&"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a & b)
    }
  }

  object Or extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Or", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IOR)
    }

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "|"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a | b)
    }
  }

  object XOr extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "XOr", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IXOR)
    }

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "^"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a ^ b)
    }
  }

  object Not extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "Not", "I").codeHandler
      ch1 << ILoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(ICONST_M1, IXOR)
    }

    override def arguments: Int = 1

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "~"

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(~a)
    }
  }

  object ShiftLeft extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "ShiftLeft", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(ISHL)
    }


    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "<<"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a << b)
    }
  }

  object ShiftUnsignedRight extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "ShiftUnsignedRight", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IUSHR)
    }

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = ">>>"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a >>> b)
    }
  }

  object ShiftSignedRight extends Function[Int] {

    override def typ: String = "I"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("I", "ShiftSignedRight", "I", "I").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(ISHR)
    }

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = ">>"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a >> b)
    }
  }

}