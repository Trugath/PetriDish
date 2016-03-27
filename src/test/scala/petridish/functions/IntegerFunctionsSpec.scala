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
class IntegerFunctionsSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  import IntegerFunctions._

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

    forAll { value: Int =>
      assert(dynObj.Nop(value) === value)
    }
  }

  "Const" should "compile and function" in {
    val cf = mkMinimalClassFile("Const")
    Const.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Const")

    forAll { value: Int =>
      assert(dynObj.Const(value) === value)
      val inst = Instruction(value).instruction(functions.indexOf(Const), Nop.instructionSize)
      val program = Program(Const.instructionSize, Seq(inst), 0, 1)
      val compiled = Compiler(program)
      assert(compiled.run() === List((value << Const.instructionSize) >> Const.instructionSize))
    }
  }

  "Add" should "compile and function" in {
    val cf = mkMinimalClassFile("Add")
    Add.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Add")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.Add(lvalue, rvalue) === (lvalue + rvalue))
    }
  }

  "Subtract" should "compile and function" in {
    val cf = mkMinimalClassFile("Subtract")
    Subtract.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Subtract")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.Subtract(lvalue, rvalue) === (lvalue - rvalue))
    }
  }

  "Multiply" should "compile and function" in {
    val cf = mkMinimalClassFile("Multiply")
    Multiply.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Multiply")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.Multiply(lvalue, rvalue) === (lvalue * rvalue))
    }
  }

  "Divide" should "compile and function" in {
    val cf = mkMinimalClassFile("Divide")
    Divide.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Divide")

    forAll { (lvalue: Int, rvalue: Int) =>
      if (rvalue != 0) {
        assert(dynObj.Divide(lvalue, rvalue) === (lvalue / rvalue))
      } else {
        assert(dynObj.Divide(lvalue, rvalue) === 0)
      }
    }
  }

  "Modulus" should "compile and function" in {
    val cf = mkMinimalClassFile("Modulus")
    Modulus.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Modulus")

    forAll { (lvalue: Int, rvalue: Int) =>
      if (rvalue != 0) {
        assert(dynObj.Modulus(lvalue, rvalue) === (lvalue % rvalue))
      } else {
        assert(dynObj.Modulus(lvalue, rvalue) === 0)
      }
    }
  }

  "Increment" should "compile and function" in {
    val cf = mkMinimalClassFile("Increment")
    Increment.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Increment")

    forAll { (value: Int) =>
      assert(dynObj.Increment(value) === (value + 1))
    }
  }

  "Decrement" should "compile and function" in {
    val cf = mkMinimalClassFile("Decrement")
    Decrement.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Decrement")

    forAll { (value: Int) =>
      assert(dynObj.Decrement(value) === (value - 1))
    }
  }

  "And" should "compile and function" in {
    val cf = mkMinimalClassFile("And")
    And.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("And")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.And(lvalue, rvalue) === (lvalue & rvalue))
    }
  }

  "Or" should "compile and function" in {
    val cf = mkMinimalClassFile("Or")
    Or.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Or")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.Or(lvalue, rvalue) === (lvalue | rvalue))
    }
  }

  "XOr" should "compile and function" in {
    val cf = mkMinimalClassFile("XOr")
    XOr.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("XOr")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.XOr(lvalue, rvalue) === (lvalue ^ rvalue))
    }
  }

  "Not" should "compile and function" in {
    val cf = mkMinimalClassFile("Not")
    Not.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Not")

    forAll { (value: Int) =>
      assert(dynObj.Not(value) === (~value))
    }
  }

  "ShiftLeft" should "compile and function" in {
    val cf = mkMinimalClassFile("ShiftLeft")
    ShiftLeft.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("ShiftLeft")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.ShiftLeft(lvalue, rvalue) === (lvalue << rvalue))
    }
  }

  "ShiftUnsignedRight" should "compile and function" in {
    val cf = mkMinimalClassFile("ShiftUnsignedRight")
    ShiftUnsignedRight.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("ShiftUnsignedRight")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.ShiftUnsignedRight(lvalue, rvalue) === (lvalue >>> rvalue))
    }
  }

  "ShiftSignedRight" should "compile and function" in {
    val cf = mkMinimalClassFile("ShiftSignedRight")
    ShiftSignedRight.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("ShiftSignedRight")

    forAll { (lvalue: Int, rvalue: Int) =>
      assert(dynObj.ShiftSignedRight(lvalue, rvalue) === (lvalue >> rvalue))
    }
  }
}
