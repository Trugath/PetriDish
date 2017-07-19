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

import evolve.core.{Generator, Program}
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class FloatCompilerSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import petridish.functions.FloatFunctions._

  val twoInputOneOutputFloatProgramRange = for {
    size <- Gen.choose[Int](2, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 2, 1, seed).shrink

  "Every two input one output integer program" should "function identically" in {
    forAll(twoInputOneOutputFloatProgramRange) { program: Program =>
      val compiled = Compiler(program)
      forAll { (lvalue: Float, rvalue: Float) =>
        val target: Float = program(List(lvalue, rvalue), 0f)._1.result(1).head
        val result: Float = compiled.run(lvalue, rvalue).asInstanceOf[Array[Float]].head
        if (!target.isNaN && !result.isNaN) {
          assert(target === result)
        }
      }
    }
  }

  val threeInputOneOutputFloatProgramRange = for {
    size <- Gen.choose[Int](3, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 3, 1, seed).shrink

  "Every three input one output integer program" should "function identically" in {
    forAll(threeInputOneOutputFloatProgramRange) { program: Program =>
      val compiled = Compiler(program)
      forAll { (a: Float, b: Float, c: Float) =>
        val target: Float = program(List(a, b, c), 0f)._1.result(1).head
        val result: Float = compiled.run(a, b, c).asInstanceOf[Array[Float]].head
        if (!target.isNaN && !result.isNaN) {
          assert(target === result)
        }
      }
    }
  }

  val twoInputTwoOutputFloatProgramRange = for {
    size <- Gen.choose[Int](2, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 2, 2, seed).shrink

  "Every two input two output integer program" should "function identically" in {
    forAll(twoInputTwoOutputFloatProgramRange) { program: Program =>
      val compiled = Compiler(program)
      forAll { (lvalue: Float, rvalue: Float) =>
        val target = program(List(lvalue, rvalue), 0f)._1.result(2)
        val result = compiled.run(lvalue, rvalue).asInstanceOf[Array[Float]]
        (target zip result).foreach {
          case (a, b) =>
            if (!a.isNaN && !b.isNaN) {
              assert(a === b)
            }
        }
      }
    }
  }

  val threeInputTwoOutputFloatProgramRange = for {
    size <- Gen.choose[Int](3, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 3, 2, seed).shrink

  "Every three input two output integer program" should "function identically" in {
    forAll(threeInputTwoOutputFloatProgramRange) { program: Program =>
      val compiled = Compiler(program)
      forAll { (a: Float, b: Float, c: Float) =>
        val target = program(List(a, b, c), 0f)._1.result(2)
        val result = compiled.run(a, b, c).asInstanceOf[Array[Float]]
        (target zip result).foreach {
          case (a, b) =>
            if (!a.isNaN && !b.isNaN) {
              assert(a === b)
            }
        }
      }
    }
  }
}
