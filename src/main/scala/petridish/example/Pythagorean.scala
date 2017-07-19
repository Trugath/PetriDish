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

package petridish.example

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.Executors

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import evolve.util.EvolveUtil
import petridish.core.{Compiler, Json}

import scala.concurrent.ExecutionContext

object Pythagorean {

  def main(args: Array[String]): Unit = {
    import Json._
    import argonaut.Argonaut._
    import petridish.functions.DoubleFunctions._

    implicit val evolveStrategy = EvolverStrategy(24, 0.0005, optimiseForPipeline = false)

    implicit val ec = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() * 2 ) )

    val testCases = TestCases(
      List(
        TestCase(List(3.0,   4.0), List(5.0)),
        TestCase(List(5.0,  12.0), List(13.0)),
        TestCase(List(8.0,  15.0), List(17.0)),
        TestCase(List(7.0,  24.0), List(25.0)),
        TestCase(List(20.0, 21.0), List(29.0)),
        TestCase(List(12.0, 35.0), List(37.0)),
        TestCase(List(9.0,  40.0), List(41.0)),
        TestCase(List(28.0, 45.0), List(53.0)),
        TestCase(List(11.0, 60.0), List(61.0)),
        TestCase(List(16.0, 63.0), List(65.0)),
        TestCase(List(48.0, 55.0), List(73.0)),
        TestCase(List(13.0, 84.0), List(85.0))
      )
    )

    val solution = EvolveUtil.fitness(Generator(Nop.instructionSize, 64, 2, 1), 0, 1000000, testCases)
    Files.write(Paths.get("solution.dot"), DotGraph(solution).getBytes(StandardCharsets.UTF_8) )
    Files.write(Paths.get("solution.json"), solution.shrink.asJson.toString().getBytes(StandardCharsets.UTF_8))

    val optimised = EvolveUtil.counted(solution.nopInputs.nopOutputs.spread(4), 50000, optimise = true, testCases).denop.shrink.deduplicate
    Files.write(Paths.get("optimised.dot"), DotGraph(optimised).getBytes(StandardCharsets.UTF_8) )
    Files.write(Paths.get("optimised.json"), optimised.asJson.toString().getBytes(StandardCharsets.UTF_8))

    val pipelined = optimised.pipeline.deduplicate.pipeline.shrink
    Files.write(Paths.get("pipelined.dot"), DotGraph(pipelined).getBytes(StandardCharsets.UTF_8) )
    Files.write(Paths.get("pipelined.json"), optimised.asJson.toString().getBytes(StandardCharsets.UTF_8))

    val cf = Compiler.classFile(optimised, "optimised")
    cf.writeToFile("optimised.class")
  }
}
