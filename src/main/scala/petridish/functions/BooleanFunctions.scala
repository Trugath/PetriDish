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
import cafebabe.ByteCodes.{IAND, ICONST_0, ICONST_1, IOR, IRETURN, IXOR, SWAP}
import cafebabe.ClassFile
import evolve.core.{Instruction, Memory}
import petridish.core.Function

object BooleanFunctions {

  implicit val functions = Seq[Function[Boolean]](
    Nop, Const,
    And, Or, Not,
    Implication, XOr, Equal
  )

  implicit def scoreFunc: (Option[Boolean], Option[Boolean]) => Long = (a, b) => {
    val result = (a, b) match {
      case (Some(left), Some(right)) => if (left == right) 0 else 10
      case (Some(left), None) => 15
      case (None, Some(right)) => 15
      case (None, None) => 0
    }
    assert(result >= 0)
    result * 100
  }

  object Nop extends Function[Boolean] {

    override def typ: String = "Z"

    override def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("Z", "Nop", "Z").codeHandler
      ch1 << ILoad(1) << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      Nil
    }

    override def arguments: Int = 1

    override def cost: Int = 1

    override def getLabel(inst: Instruction): String = "Nop"

    override def apply(inst: Instruction, memory: Memory[Boolean]): Memory[Boolean] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a)
    }
  }

  object Const extends Function[Boolean] {

    override def typ: String = "Z"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("Z", "Const", "Z").codeHandler
      ch1 << ILoad(1) << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(if (inst.const(instructionSize, 1) == -1) ICONST_1 else ICONST_0)
    }

    override def arguments: Int = 0

    override def cost: Int = 1

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 1) == -1
      s"Const ($value)"
    }

    override def apply(inst: Instruction, memory: Memory[Boolean]): Memory[Boolean] = {
      memory.append(inst.const(instructionSize, 1) == -1)
    }
  }

  object And extends Function[Boolean] {

    override def typ: String = "Z"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("Z", "And", "Z", "Z").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IAND)
    }

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = "&"

    override def apply(inst: Instruction, memory: Memory[Boolean]): Memory[Boolean] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a & b)
    }
  }

  object Or extends Function[Boolean] {

    override def typ: String = "Z"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("Z", "Or", "Z", "Z").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IOR)
    }

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = "|"

    override def apply(inst: Instruction, memory: Memory[Boolean]): Memory[Boolean] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a | b)
    }
  }

  object Not extends Function[Boolean] {

    override def typ: String = "Z"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("Z", "Not", "Z").codeHandler
      ch1 << ILoad(1)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(ICONST_1, IXOR)
    }

    override def arguments: Int = 1

    override def cost: Int = 2

    override def getLabel(inst: Instruction): String = "~"

    override def apply(inst: Instruction, memory: Memory[Boolean]): Memory[Boolean] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(!a)
    }
  }

  object Implication extends Function[Boolean] {

    override def typ: String = "Z"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("Z", "Implication", "Z", "Z").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(SWAP, ICONST_1, IXOR, IOR)
    }

    override def cost: Int = 20

    // we want (!a | b) to be cheaper
    override def getLabel(inst: Instruction): String = "->"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[Boolean]): Memory[Boolean] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(!a || b)
    }
  }

  object XOr extends Function[Boolean] {

    override def typ: String = "Z"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("Z", "XOr", "Z", "Z").codeHandler
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

    // we want to be cheaper than the equivalent basic gate setup
    override def getLabel(inst: Instruction): String = "^"

    override def apply(inst: Instruction, memory: Memory[Boolean]): Memory[Boolean] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a ^ b)
    }
  }

  object Equal extends Function[Boolean] {

    override def typ: String = "Z"

    def addToClass(cf: ClassFile): ClassFile = {
      val ch1 = cf.addMethod("Z", "Equal", "Z", "Z").codeHandler
      ch1 << ILoad(1) << ILoad(2)
      compile(Instruction(0)).foreach(bc => ch1 << bc)
      ch1 << IRETURN
      ch1.freeze()
      cf
    }

    def compile(inst: Instruction): List[AbstractByteCode] = {
      List(IXOR, ICONST_1, IXOR)
    }

    override def cost: Int = 20

    // we want XOR -> NOT to be cheaper
    override def getLabel(inst: Instruction): String = "=="

    override def apply(inst: Instruction, memory: Memory[Boolean]): Memory[Boolean] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a == b)
    }
  }

}
