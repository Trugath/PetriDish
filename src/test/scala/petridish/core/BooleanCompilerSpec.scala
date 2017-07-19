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

import evolve.core.{Generator, Instruction, Program}
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

/**
  * Created by Elliot on 22/03/2016.
  */
class BooleanCompilerSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import petridish.functions.BooleanFunctions._

  "A basic program" should "compile" in {

    val booleanAnd: Program = Program(And.instructionSize,
      Seq(
        Instruction(functions.indexOf(And), And.instructionSize)
          .pointer(0, And.instructionSize, And.argumentSize)
          .pointer(1, And.instructionSize + And.argumentSize, And.argumentSize)
      ), 2, 1)

    assert(booleanAnd(List(false, false), false)._1.result(3) === List(false, false, false))
    assert(booleanAnd(List(false, true), false)._1.result(3) === List(false, true, false))
    assert(booleanAnd(List(true, false), false)._1.result(3) === List(true, false, false))
    assert(booleanAnd(List(true, true), false)._1.result(3) === List(true, true, true))

    val prog = Compiler(booleanAnd)
    assert(prog.run(false, false) === List(false))
    assert(prog.run(false, true) === List(false))
    assert(prog.run(true, false) === List(false))
    assert(prog.run(true, true) === List(true))
  }

  "A slightly more complex program" should "compile" in {
    val booleanAnd: Program = Program(And.instructionSize,
      Seq(
        Instruction(functions.indexOf(Not), Not.instructionSize)
          .pointer(0, Not.instructionSize, Not.argumentSize),
        Instruction(functions.indexOf(Not), Not.instructionSize)
          .pointer(1, Not.instructionSize, Not.argumentSize),
        Instruction(functions.indexOf(And), And.instructionSize)
          .pointer(0, And.instructionSize, And.argumentSize)
          .pointer(3, And.instructionSize + And.argumentSize, And.argumentSize),
        Instruction(functions.indexOf(And), And.instructionSize)
          .pointer(1, And.instructionSize, And.argumentSize)
          .pointer(2, And.instructionSize + And.argumentSize, And.argumentSize),
        Instruction(functions.indexOf(Or), Or.instructionSize)
          .pointer(4, Or.instructionSize, Or.argumentSize)
          .pointer(5, Or.instructionSize + Or.argumentSize, Or.argumentSize)
      ), 2, 1)

    assert(booleanAnd(List(false, false), false)._1.result(1) === List(false))
    assert(booleanAnd(List(false, true), false)._1.result(1) === List(true))
    assert(booleanAnd(List(true, false), false)._1.result(1) === List(true))
    assert(booleanAnd(List(true, true), false)._1.result(1) === List(false))

    val prog = Compiler(booleanAnd)
    assert(prog.run(false, false) === List(false))
    assert(prog.run(false, true) === List(true))
    assert(prog.run(true, false) === List(true))
    assert(prog.run(true, true) === List(false))
  }

  "Program(6,Vector(Instruction(335552512), Instruction(67108864), Instruction(335544322), Instruction(335577091), Instruction(40960)),2,1)" should "Compile and function" in {
    val program = Program(6, Vector(Instruction(335552512), Instruction(67108864), Instruction(335544322), Instruction(335577091), Instruction(40960)), 2, 1)
    val compiled = Compiler(program)
    assert(program(List(false, false), false)._1.result(1) === compiled.run(false, false))
    assert(program(List(false, true), false)._1.result(1) === compiled.run(false, true))
    assert(program(List(true, false), false)._1.result(1) === compiled.run(true, false))
    assert(program(List(true, true), false)._1.result(1) === compiled.run(true, true))
  }

  "Program(6,Vector(Instruction(0), Instruction(0), Instruction(134242304), Instruction(134250498), Instruction(201367555)),2,1)" should "Compile and Function" in {
    val program = Program(6, Vector(Instruction(0), Instruction(0), Instruction(134242304), Instruction(134250498), Instruction(201367555)), 2, 1)
    val compiled = Compiler(program)
    assert(program(List(false, false), false)._1.result(1) === compiled.run(false, false))
    assert(program(List(false, true), false)._1.result(1) === compiled.run(false, true))
    assert(program(List(true, false), false)._1.result(1) === compiled.run(true, false))
    assert(program(List(true, true), false)._1.result(1) === compiled.run(true, true))
  }

  "Program(6,Vector(Instruction(67108864), Instruction(67108864), Instruction(67108864), Instruction(134234112), Instruction(469794820), Instruction(402694149), Instruction(134258695), Instruction(201392134), Instruction(402677769)),2,1)" should "Compile and function" in {
    val program = Program(6, Vector(Instruction(67108864), Instruction(67108864), Instruction(67108864), Instruction(134234112), Instruction(469794820), Instruction(402694149), Instruction(134258695), Instruction(201392134), Instruction(402677769)), 2, 1)
    val compiled = Compiler(program)
    assert(program(List(false, false), false)._1.result(1) === compiled.run(false, false))
    assert(program(List(false, true), false)._1.result(1) === compiled.run(false, true))
    assert(program(List(true, false), false)._1.result(1) === compiled.run(true, false))
    assert(program(List(true, true), false)._1.result(1) === compiled.run(true, true))
  }

  val twoInputOneOutputBooleanProgramRange = for {
    size <- Gen.choose[Int](2, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 2, 1, seed).shrink

  "Every two input one output boolean program" should "function identically" in {
    forAll(twoInputOneOutputBooleanProgramRange) { program: Program =>
      val compiled = Compiler(program)
      assert(program(List(false, false), false)._1.result(1) === compiled.run(false, false))
      assert(program(List(false, true), false)._1.result(1) === compiled.run(false, true))
      assert(program(List(true, false), false)._1.result(1) === compiled.run(true, false))
      assert(program(List(true, true), false)._1.result(1) === compiled.run(true, true))
    }
  }

  val threeInputOneOutputBooleanProgramRange = for {
    size <- Gen.choose[Int](3, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 3, 1, seed).shrink

  "Every three input one output boolean program" should "function identically" in {
    forAll(threeInputOneOutputBooleanProgramRange) { program: Program =>
      val compiled = Compiler(program)
      assert(program(List(false, false, false), false)._1.result(1) === compiled.run(false, false, false))
      assert(program(List(false, false, true), false)._1.result(1) === compiled.run(false, false, true))
      assert(program(List(false, true, false), false)._1.result(1) === compiled.run(false, true, false))
      assert(program(List(false, true, true), false)._1.result(1) === compiled.run(false, true, true))
      assert(program(List(true, false, false), false)._1.result(1) === compiled.run(true, false, false))
      assert(program(List(true, false, true), false)._1.result(1) === compiled.run(true, false, true))
      assert(program(List(true, true, false), false)._1.result(1) === compiled.run(true, true, false))
      assert(program(List(true, true, true), false)._1.result(1) === compiled.run(true, true, true))
    }
  }


  val twoInputTwoOutputBooleanProgramRange = for {
    size <- Gen.choose[Int](2, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 2, 2, seed).shrink

  "Every two input two output boolean program" should "function identically" in {
    forAll(twoInputTwoOutputBooleanProgramRange) { program: Program =>
      val compiled = Compiler(program)
      assert(program(List(false, false), false)._1.result(2) === compiled.run(false, false))
      assert(program(List(false, true), false)._1.result(2) === compiled.run(false, true))
      assert(program(List(true, false), false)._1.result(2) === compiled.run(true, false))
      assert(program(List(true, true), false)._1.result(2) === compiled.run(true, true))
    }
  }

  val threeInputTwoOutputBooleanProgramRange = for {
    size <- Gen.choose[Int](3, math.pow(2, Nop.instructionSize).floor.toInt)
    seed <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Generator(Nop.instructionSize, size, 3, 2, seed).shrink

  "Every three input two output boolean program" should "function identically" in {
    forAll(threeInputTwoOutputBooleanProgramRange) { program: Program =>
      val compiled = Compiler(program)
      assert(program(List(false, false, false), false)._1.result(2) === compiled.run(false, false, false))
      assert(program(List(false, false, true), false)._1.result(2) === compiled.run(false, false, true))
      assert(program(List(false, true, false), false)._1.result(2) === compiled.run(false, true, false))
      assert(program(List(false, true, true), false)._1.result(2) === compiled.run(false, true, true))
      assert(program(List(true, false, false), false)._1.result(2) === compiled.run(true, false, false))
      assert(program(List(true, false, true), false)._1.result(2) === compiled.run(true, false, true))
      assert(program(List(true, true, false), false)._1.result(2) === compiled.run(true, true, false))
      assert(program(List(true, true, true), false)._1.result(2) === compiled.run(true, true, true))
    }
  }
}
