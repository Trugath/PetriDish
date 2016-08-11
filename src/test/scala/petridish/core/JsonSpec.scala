package petridish
package core

import evolve.core.{Generator, Program}
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

/**
  * Created by Elliot on 11/08/2016.
  */
class JsonSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import argonaut.Argonaut._
  import Json._

  "Any program" should "serialise and deserialise to JSON" in {
    forAll(Gen.choose[Int](1, 64), Gen.choose[Int](0, 16), Gen.choose[Int](1, 16), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (size: Int, inputCount: Int, outputCount: Int, seed: Int) => whenever(size >= outputCount && seed != 0) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed)
          val decoded = program.asJson.toString().decodeOption[Program].get
          assert( program === decoded )
        }
        {
          import functions.DoubleFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed)
          val decoded = program.asJson.toString().decodeOption[Program].get
          assert( program === decoded )
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed)
          val decoded = program.asJson.toString().decodeOption[Program].get
          assert( program === decoded )
        }
      }
    }
  }
}
