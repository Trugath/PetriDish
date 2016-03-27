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

class IntegerCompilerSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import petridish.functions.IntegerFunctions._

  val twoInputOneOutputIntegerProgramRange = for {
    size <- Gen.choose[Int](2, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 2, 1, seed).shrink

  "Every two input one output integer program" should "function identically" in {
    forAll(twoInputOneOutputIntegerProgramRange) { program: Program =>
      val compiled = Compiler(program)
      forAll { (lvalue: Int, rvalue: Int) =>
        assert(program(List(lvalue, rvalue)).result(1) === compiled.run(lvalue, rvalue))
      }
    }
  }

  val threeInputOneOutputIntegerProgramRange = for {
    size <- Gen.choose[Int](3, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 3, 1, seed).shrink

  "Every three input one output integer program" should "function identically" in {
    forAll(threeInputOneOutputIntegerProgramRange) { program: Program =>
      val compiled = Compiler(program)
      forAll { (a: Int, b: Int, c: Int) =>
        assert(program(List(a, b, c)).result(1) === compiled.run(a, b, c))
      }
    }
  }

  val twoInputTwoOutputIntegerProgramRange = for {
    size <- Gen.choose[Int](2, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 2, 2, seed).shrink

  "Every two input two output integer program" should "function identically" in {
    forAll(twoInputTwoOutputIntegerProgramRange) { program: Program =>
      val compiled = Compiler(program)
      forAll { (lvalue: Int, rvalue: Int) =>
        assert(program(List(lvalue, rvalue)).result(2) === compiled.run(lvalue, rvalue))
      }
    }
  }

  val threeInputTwoOutputIntegerProgramRange = for {
    size <- Gen.choose[Int](3, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 3, 2, seed).shrink

  "Every three input two output integer program" should "function identically" in {
    forAll(threeInputTwoOutputIntegerProgramRange) { program: Program =>
      val compiled = Compiler(program)
      forAll { (a: Int, b: Int, c: Int) =>
        assert(program(List(a, b, c)).result(2) === compiled.run(a, b, c))
      }
    }
  }
}
