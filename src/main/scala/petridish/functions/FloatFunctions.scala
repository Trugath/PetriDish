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
import evolve.core.{Instruction, Memory}
import petridish.core.Function


object FloatFunctions {

  import Function._

  implicit val functions = Seq[Function[Float]](
    Nop,
    ConstLarge, ConstSmall,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement
  )

  implicit def scoreFunc: (Float, Float) => Long = (a, b) => {

    def nabs(i: Float): Float = if (i < 0) -i else i

    val result: Float = (a, b) match {
      case (left, right) if left.isNaN || right.isNaN => Int.MaxValue
      case (left, right)                              => nabs(left - right).abs
    }
    assert(result >= -0.00001)
    math.min(result * Int.MaxValue, Long.MaxValue / 256L).toLong
  }

  object Nop extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Nop", "F").codeHandler
      ch1 << FLoad(1)
      ch1 << compile(Instruction(0))
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      Nil
    }

    override val arguments: Int = 1

    override val cost: Int = 2

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

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      val i = inst.const(instructionSize, 32 - instructionSize)
      def const(value: Int): AbstractByteCodeGenerator = {
        value match {
          case 0 => List(ICONST_0)
          case 1 => List(ICONST_1)
          case 2 => List(ICONST_2)
          case 3 => List(ICONST_3)
          case 4 => List(ICONST_4)
          case 5 => List(ICONST_5)
          case _ if value >= -128 && value <= 127 => List(BIPUSH, RawByte(value.asInstanceOf[U1]))
          case _ if value >= -32768 && value <= 32767 => List(SIPUSH, RawBytes(value.asInstanceOf[U2]))
          case _ => const(value & 0x7fff) andThen const(value >> 15) andThen const(15) andThen List(ISHL, IOR)
        }
      }

      const(i) andThen List(I2F)
    }

    override val arguments: Int = 0

    override val constantRegionSize: Int = 32 - constantRegionStart

    override val cost: Int = 2

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(constantRegionStart, constantRegionSize)
      s"Const ($value)"
    }

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      inst.const(constantRegionStart, constantRegionSize)
    }
  }

  object ConstSmall extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "ConstSmall", "F").codeHandler
      ch1 << FLoad(1) << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      val i = inst.const(instructionSize, 32 - instructionSize)
      def const(value: Int): AbstractByteCodeGenerator = {
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
          case _ => const(value & 0x7fff) andThen const(value >> 15) andThen const(15) andThen List(ISHL, IOR)
        }
      }
      i match {
        case 0 => FCONST_0 andThen const(scale.toInt) andThen List(I2F, FDIV)
        case 1 => FCONST_1 andThen const(scale.toInt) andThen List(I2F, FDIV)
        case 2 => FCONST_2 andThen const(scale.toInt) andThen List(I2F, FDIV)
        case _ => const(i) andThen List(I2F) andThen const(scale.toInt) andThen List(I2F, FDIV)
      }
    }

    override val arguments: Int = 0

    override val constantRegionSize: Int = 32 - constantRegionStart

    private [this] val scale: Float = math.pow(2.0, constantRegionSize).toFloat

    override val cost: Int = 2

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(constantRegionStart, constantRegionSize) / scale
      s"Const ($value)"
    }

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      inst.const(constantRegionStart, constantRegionSize) / scale
    }
  }

  object Add extends Function[Float] {

    override def typ: String = "F"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("F", "Add", "F", "F").codeHandler
      ch1 << FLoad(1) << FLoad(2)
      ch1 << compile(Instruction(0))
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      FADD
    }

    override val cost: Int = 4

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
      ch1 << compile(Instruction(0))
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      FSUB
    }

    override val cost: Int = 4

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
      ch1 << compile(Instruction(0))
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      FMUL
    }

    override val cost: Int = 5

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
      ch1 << compile(Instruction(0))
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      FDIV
    }

    override val cost: Int = 10

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
      ch1 << compile(Instruction(0))
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      FREM
    }

    override val cost: Int = 10

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
      ch1 << compile(Instruction(0))
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      List(FCONST_1, FADD)
    }

    override val arguments: Int = 1

    override val cost: Int = 3

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
      ch1 << compile(Instruction(0))
      ch1 << FRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): AbstractByteCodeGenerator = {
      List(FCONST_1, FSUB)
    }

    override val arguments: Int = 1

    override val cost: Int = 3

    override def getLabel(inst: Instruction): String = "Decrement"

    override def apply(inst: Instruction, arguments: List[Float]): Float = {
      val a = arguments.head
      a - 1
    }
  }

}
