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


object DoubleFunctions {

  implicit val functions = Seq[Function[Double]](
    Nop,
    ConstLarge, ConstSmall,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement
  )

  implicit def scoreFunc: (Option[Double], Option[Double]) => Long = (a, b) => {

    def nabs(i: Double): Double = if (i < 0) -i else i

    val result: Double = (a, b) match {
      case (Some(left), Some(right)) if left.isNaN || right.isNaN => Int.MaxValue
      case (Some(left), Some(right)) => nabs(left - right).abs
      case (Some(left), _) => left.abs
      case (_, Some(right)) => right.abs
      case (_, _) => 0
    }
    assert(result >= -0.00001)
    math.min(result * Int.MaxValue, Long.MaxValue / 256L).toLong
  }

  object Nop extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "Nop", "D").codeHandler
      ch1 << DLoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << DRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      Nil
    }

    override def arguments: Int = 1

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = "Nop"

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a)
    }
  }

  object ConstLarge extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "ConstLarge", "D").codeHandler
      ch1 << DLoad(1) << DRETURN
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

      const(i) ::: I2D :: Nil
    }

    override def arguments: Int = 0

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize)
      s"Const ($value)"
    }

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      memory.append(inst.const(instructionSize, 32 - instructionSize))
    }
  }

  object ConstSmall extends Function[Double] {

    private val scale: Double = math.pow(2.0, 32 - instructionSize).toDouble

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "ConstSmall", "D").codeHandler
      ch1 << DLoad(1) << DRETURN
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
        case 0 => DCONST_0 :: const(scale.toInt) ::: I2D :: DDIV :: Nil
        case 1 => DCONST_1 :: const(scale.toInt) ::: I2D :: DDIV :: Nil
        case _ => const(i) ::: I2D :: const(scale.toInt) ::: I2D :: DDIV :: Nil
      }
    }

    override def arguments: Int = 0

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize) / scale
      s"Const ($value)"
    }

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      memory.append(inst.const(instructionSize, 32 - instructionSize) / scale)
    }
  }

  object Add extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "Add", "D", "D").codeHandler
      ch1 << DLoad(1) << DLoad(3)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << DRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(DADD)
    }

    override def cost: Int = 4

    override def getLabel(inst: Instruction): String = "Add"

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a + b)
    }
  }

  object Subtract extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "Subtract", "D", "D").codeHandler
      ch1 << DLoad(1) << DLoad(3)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << DRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(DSUB)
    }

    override def cost: Int = 4

    override def getLabel(inst: Instruction): String = "Subtract"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a - b)
    }
  }

  object Multiply extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "Multiply", "D", "D").codeHandler
      ch1 << DLoad(1) << DLoad(3)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << DRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(DMUL)
    }

    override def cost: Int = 5

    override def getLabel(inst: Instruction): String = "Multiply"

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a * b)
    }
  }

  object Divide extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "Divide", "D", "D").codeHandler
      ch1 << DLoad(1) << DLoad(3)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << DRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(DDIV)
    }

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Divide"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      try {
        memory.append(a / b)
      } catch {
        case e: ArithmeticException => memory.append(0)
      }
    }
  }

  object Modulus extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "Modulus", "D", "D").codeHandler
      ch1 << DLoad(1) << DLoad(3)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << DRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(DREM)
    }

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Modulus"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      try {
        memory.append(a % b)
      } catch {
        case e: ArithmeticException => memory.append(0)
      }
    }
  }

  object Increment extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "Increment", "D").codeHandler
      ch1 << DLoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << DRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(DCONST_1, DADD)
    }

    override def arguments: Int = 1

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "Increment"

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a + 1)
    }
  }

  object Decrement extends Function[Double] {

    override def typ: String = "D"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("D", "Decrement", "D").codeHandler
      ch1 << DLoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << DRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(DCONST_1, DSUB)
    }

    override def arguments: Int = 1

    override def cost: Int = 3

    override def getLabel(inst: Instruction): String = "Decrement"

    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a - 1)
    }
  }

}
