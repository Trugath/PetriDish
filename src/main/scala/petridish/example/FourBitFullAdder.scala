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

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

object FourBitFullAdder {

  def main(args: Array[String]): Unit = {

    import petridish.functions.BooleanFunctions._

    implicit val evolverStrategy = EvolverStrategy(24, 0.00015, optimiseForPipeline = true)
    implicit val ec = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() ) )

    def bitsToBools(value: Int, bits: Int): List[Boolean] = {
      require(value >= 0 && value <= math.pow(2, bits))
      (0 until bits)
        .map(i => ((0x1 << i) & value) != 0x0)
        .reverse
        .toList
    }

    val testCases = TestCases((for {
      l <- 0 until 16
      r <- 0 until 16
    } yield TestCase[Boolean](bitsToBools(l, 4) ::: bitsToBools(r, 4), bitsToBools(l + r, 5))).toList)

    @tailrec def function(program: Program, generation: Long, improvements: Long): Program = {
      Evolver(program, testCases, optimise = false) match {
        case Some(evolved) =>
          val score = testCases.score(evolved)
          if (score == 0) {
            println( s"Solution found after $generation generations with $improvements mutations." )
            evolved
          } else {
            function(evolved, generation + 1, improvements + 1)
          }

        case None =>
          function(program, generation + 1, improvements)
      }
    }

    import Json._
    import argonaut.Argonaut._

    val start: Program = {
      val path = Paths.get("pipelined.json")
      (if (Files.exists(path)) {
        Files
          .readAllLines(path, StandardCharsets.UTF_8)
          .toArray
          .mkString
          .decodeOption[Program]
      } else None) match {
        case Some(program: Program) if program.inputCount == 8 && program.outputCount == 5 =>
          assert( testCases.score( program ) == 0 )
          program.pipeline.grow(256)
        case _                   =>
          Generator(Nop.instructionSize, 256, 8, 5)
      }
    }

    val solution = function(EvolveUtil.counted(start, 2000, optimise = false, testCases), 0, 0).denop.deduplicate.shrink
    assert( testCases.score( solution ) == 0 )
    Files.write(Paths.get("solution.dot"), DotGraph(solution).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("solution.json"), solution.shrink.asJson.toString().getBytes(StandardCharsets.UTF_8))

    val optimised = EvolveUtil.counted(solution, 20000, optimise = true, testCases).denop.deduplicate.shrink
    assert( testCases.score( optimised ) == 0 )
    Files.write(Paths.get("optimised.dot"), DotGraph(optimised).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("optimised.json"), optimised.asJson.toString().getBytes(StandardCharsets.UTF_8))

    val pipelined = optimised.pipeline.deduplicate.pipeline.shrink
    Files.write(Paths.get("pipelined.dot"), DotGraph(pipelined).getBytes(StandardCharsets.UTF_8) )
    Files.write(Paths.get("pipelined.json"), optimised.asJson.toString().getBytes(StandardCharsets.UTF_8))

    val cf = Compiler.classFile(optimised, "optimised")
    cf.writeToFile("optimised.class")

    System.gc()
    System.exit(0)
  }
}
