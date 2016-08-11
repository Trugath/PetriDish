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
import cafebabe.ClassFileTypes._
import evolve.core.Memory.ZeroValueMemory
import evolve.core.{Instruction, Memory}
import petridish.core.Function


object FloatFunctions {

  implicit val zero = ZeroValueMemory[Float]( 0f )

  implicit val functions = Seq[Function[Float]](
    Nop,
    ConstLarge, ConstSmall,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement
  )

  implicit def scoreFunc: (Option[Float], Option[Float]) => Long = (a, b) => {

    def nabs(i: Float): Float = if (i < 0) -i else i

    val result: Float = (a, b) match {
      case (Some(left), Some(right)) if left.isNaN || right.isNaN => Int.MaxValue
      case (Some(left), Some(right)) => nabs(left - right).abs
      case (Some(left), _) => left.abs
      case (_, Some(right)) => right.abs
      case (_, _) => 0
    }
    assert(result >= -0.00001)
    math.min(result * Int.MaxValue, Long.MaxValue / 256L).toLong
  }

  object Nop extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Nop", "F").codeHandler
      ch1 << FLoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      Nil
    }

    override def arguments: Int = 1

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = "Nop"

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      arguments.head
    }
  }

  object ConstLarge extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "ConstLarge", "F").codeHandler
      ch1 << FLoad(1) << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      val i = inst.const(instructionSize, 32 - instructionSize)
      def const(value: Int): List[AbstractByteCode] = {
        value match {
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

      const(i) ::: I2F :: Nil
    }

    override def arguments: Int = 0

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize)
      s"Const ($value)"
    }

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      inst.const(instructionSize, 32 - instructionSize)
    }
  }

  object ConstSmall extends Function[Float] {

    private val scale: Float = math.pow(2.0, 32 - instructionSize).toFloat

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "ConstSmall", "F").codeHandler
      ch1 << FLoad(1) << FRETURN
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
      i match {
        case 0 => FCONST_0 :: const(scale.toInt) ::: I2F :: FDIV :: Nil
        case 1 => FCONST_1 :: const(scale.toInt) ::: I2F :: FDIV :: Nil
        case 2 => FCONST_2 :: const(scale.toInt) ::: I2F :: FDIV :: Nil
        case _ => const(i) ::: I2F :: const(scale.toInt) ::: I2F :: FDIV :: Nil
      }
    }

    override def arguments: Int = 0

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize) / scale
      s"Const ($value)"
    }

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      inst.const(instructionSize, 32 - instructionSize) / scale
    }
  }

  object Add extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Add", "F", "F").codeHandler
      ch1 << FLoad(1) << FLoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(FADD)
    }

    override def cost: Int = 4

    override def getLabel(inst: Instruction): String = "Add"

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      val a = arguments.head
      val b = arguments(1)
      a + b
    }
  }

  object Subtract extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Subtract", "F", "F").codeHandler
      ch1 << FLoad(1) << FLoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(FSUB)
    }

    override def cost: Int = 4

    override def getLabel(inst: Instruction): String = "Subtract"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      val a = arguments.head
      val b = arguments(1)
      a - b
    }
  }

  object Multiply extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Multiply", "F", "F").codeHandler
      ch1 << FLoad(1) << FLoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(FMUL)
    }

    override def cost: Int = 5

    override def getLabel(inst: Instruction): String = "Multiply"

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      val a = arguments.head
      val b = arguments(1)
      a * b
    }
  }

  object Divide extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Divide", "F", "F").codeHandler
      ch1 << FLoad(1) << FLoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(FDIV)
    }

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Divide"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      val a = arguments.head
      val b = arguments(1)
      try {
        a / b
      } catch {
        case e: ArithmeticException => 0
      }
    }
  }

  object Modulus extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Modulus", "F", "F").codeHandler
      ch1 << FLoad(1) << FLoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(FREM)
    }

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Modulus"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      val a = arguments.head
      val b = arguments(1)
      try {
        a % b
      } catch {
        case e: ArithmeticException => 0
      }
    }
  }

  object Increment extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Increment", "F").codeHandler
      ch1 << FLoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(FCONST_1, FADD)
    }

    override def arguments: Int = 1

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "Increment"

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      val a = arguments.head
      a + 1
    }
  }

  object Decrement extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Decrement", "F").codeHandler
      ch1 << FLoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(FCONST_1, FSUB)
    }

    override def arguments: Int = 1

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "Decrement"

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      val a = arguments.head
      a - 1
    }
  }

}
