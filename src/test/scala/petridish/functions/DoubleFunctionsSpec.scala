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

import cafebabe.{CafebabeClassLoader, ClassFile}
import evolve.core.{Instruction, Program}
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import petridish.core.Compiler

/**
  * Created by Elliot on 22/05/2015.
  */
class DoubleFunctionsSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  import DoubleFunctions._

  private def mkMinimalClassFile(name: String, parent: Option[String] = None): ClassFile = {
    val cf = new ClassFile(name, parent)
    cf.addDefaultConstructor()
    cf
  }

  "Nop" should "compile and function" in {
    val cf = mkMinimalClassFile("Nop")
    Nop.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Nop")

    forAll { value: Double =>
      assert(dynObj.Nop(value) === value)
    }
  }

  "ConstLarge" should "compile and function" in {
    val cf = mkMinimalClassFile("ConstLarge")
    ConstLarge.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("ConstLarge")

    forAll { value: Int =>
      assert(dynObj.ConstLarge(value) === value)
      val inst = Instruction(value).instruction(functions.indexOf(ConstLarge), ConstLarge.instructionSize)
      val program = Program(ConstLarge.instructionSize, Seq(inst), 0, 1)
      val compiled = Compiler(program)
      assert(compiled.run() === List(((value << ConstSmall.instructionSize) >> ConstSmall.instructionSize).toDouble))
    }
  }

  "ConstSmall" should "compile and function" in {
    val cf = mkMinimalClassFile("ConstSmall")
    ConstSmall.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("ConstSmall")

    val scale: Double = math.pow(2.0, 32 - ConstSmall.instructionSize).toDouble

    forAll { value: Int =>
      assert(dynObj.ConstSmall(value.toDouble) === value.toDouble)
      val inst = Instruction(value).instruction(functions.indexOf(ConstSmall), ConstSmall.instructionSize)
      val program = Program(ConstSmall.instructionSize, Seq(inst), 0, 1)
      val compiled = Compiler(program)
      assert(compiled.run() === List(((value << ConstSmall.instructionSize) >> ConstSmall.instructionSize).toDouble / scale))
    }
  }

  "Add" should "compile and function" in {
    val cf = mkMinimalClassFile("Add")
    Add.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Add")

    val inst = Instruction(0)
      .instruction(functions.indexOf(Add), Add.instructionSize)
      .pointer(0, Add.instructionSize, Add.argumentSize)
      .pointer(1, Add.instructionSize + Add.argumentSize, Add.argumentSize)

    val program = Program(ConstSmall.instructionSize, Seq(inst), 2, 1)
    val compiled = Compiler(program)

    forAll { (lvalue: Double, rvalue: Double) =>
      assert(dynObj.Add(lvalue, rvalue) === (lvalue + rvalue))
      assert(compiled.run(lvalue, rvalue) === List(lvalue + rvalue))
    }
  }

  "Subtract" should "compile and function" in {
    val cf = mkMinimalClassFile("Subtract")
    Subtract.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Subtract")

    val inst = Instruction(0)
      .instruction(functions.indexOf(Subtract), Subtract.instructionSize)
      .pointer(0, Subtract.instructionSize, Subtract.argumentSize)
      .pointer(1, Subtract.instructionSize + Subtract.argumentSize, Subtract.argumentSize)

    val program = Program(Subtract.instructionSize, Seq(inst), 2, 1)
    val compiled = Compiler(program)

    forAll { (lvalue: Double, rvalue: Double) =>
      assert(dynObj.Subtract(lvalue, rvalue) === (lvalue - rvalue))
      assert(compiled.run(lvalue, rvalue) === List(lvalue - rvalue))
    }
  }

  "Multiply" should "compile and function" in {
    val cf = mkMinimalClassFile("Multiply")
    Multiply.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Multiply")

    val inst = Instruction(0)
      .instruction(functions.indexOf(Multiply), Multiply.instructionSize)
      .pointer(0, Multiply.instructionSize, Multiply.argumentSize)
      .pointer(1, Multiply.instructionSize + Multiply.argumentSize, Multiply.argumentSize)

    val program = Program(Multiply.instructionSize, Seq(inst), 2, 1)
    val compiled = Compiler(program)

    forAll { (lvalue: Double, rvalue: Double) =>
      assert(dynObj.Multiply(lvalue, rvalue) === (lvalue * rvalue))
      assert(compiled.run(lvalue, rvalue) === List(lvalue * rvalue))
    }
  }

  "Divide" should "compile and function" in {
    val cf = mkMinimalClassFile("Divide")
    Divide.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Divide")

    val inst = Instruction(0)
      .instruction(functions.indexOf(Divide), Divide.instructionSize)
      .pointer(0, Divide.instructionSize, Divide.argumentSize)
      .pointer(1, Divide.instructionSize + Divide.argumentSize, Divide.argumentSize)

    val program = Program(Divide.instructionSize, Seq(inst), 2, 1)
    val compiled = Compiler(program)

    forAll { (lvalue: Double, rvalue: Double) =>
      if (rvalue != 0) {
        assert(dynObj.Divide(lvalue, rvalue) === (lvalue / rvalue))
        assert(compiled.run(lvalue, rvalue) === List(lvalue / rvalue))
      } else {
        assert(dynObj.Divide(lvalue, rvalue) === 0)
        assert(compiled.run(lvalue, rvalue) === List(0))
      }
    }
  }

  "Modulus" should "compile and function" in {
    val cf = mkMinimalClassFile("Modulus")
    Modulus.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Modulus")

    val inst = Instruction(0)
      .instruction(functions.indexOf(Modulus), Modulus.instructionSize)
      .pointer(0, Modulus.instructionSize, Modulus.argumentSize)
      .pointer(1, Modulus.instructionSize + Modulus.argumentSize, Modulus.argumentSize)

    val program = Program(Modulus.instructionSize, Seq(inst), 2, 1)
    val compiled = Compiler(program)

    forAll { (lvalue: Double, rvalue: Double) =>
      if (rvalue != 0) {
        assert(dynObj.Modulus(lvalue, rvalue) === (lvalue % rvalue))
        assert(compiled.run(lvalue, rvalue) === List(lvalue % rvalue))
      } else {
        assert(dynObj.Modulus(lvalue, rvalue) === 0)
        assert(compiled.run(lvalue, rvalue) === List(0))
      }
    }
  }

  "Increment" should "compile and function" in {
    val cf = mkMinimalClassFile("Increment")
    Increment.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Increment")

    val inst = Instruction(0)
      .instruction(functions.indexOf(Increment), Increment.instructionSize)
      .pointer(0, Increment.instructionSize, Increment.argumentSize)
      .pointer(1, Increment.instructionSize + Increment.argumentSize, Increment.argumentSize)

    val program = Program(Increment.instructionSize, Seq(inst), 1, 1)
    val compiled = Compiler(program)

    forAll { (value: Double) =>
      assert(dynObj.Increment(value) === (value + 1))
      assert(compiled.run(value) === List(value + 1))
    }
  }

  "Decrement" should "compile and function" in {
    val cf = mkMinimalClassFile("Decrement")
    Decrement.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Decrement")

    val inst = Instruction(0)
      .instruction(functions.indexOf(Decrement), Decrement.instructionSize)
      .pointer(0, Decrement.instructionSize, Decrement.argumentSize)
      .pointer(1, Decrement.instructionSize + Decrement.argumentSize, Decrement.argumentSize)

    val program = Program(Decrement.instructionSize, Seq(inst), 1, 1)
    val compiled = Compiler(program)

    forAll { (value: Double) =>
      assert(dynObj.Decrement(value) === (value - 1))
      assert(compiled.run(value) === List(value - 1))
    }
  }
}
