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
import org.scalatest.FlatSpec

/**
  * Created by Elliot on 22/05/2015.
  */
class BooleanFunctionsSpec extends FlatSpec {

  import BooleanFunctions._

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

    assert(dynObj.Nop(true) === true)
    assert(dynObj.Nop(false) === false)
  }

  "Const" should "compile and function" in {
    val cf = mkMinimalClassFile("Const")
    Const.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Const")

    assert(dynObj.Const(true) === true)
    assert(dynObj.Const(false) === false)
  }

  "And" should "compile and function" in {
    val cf = mkMinimalClassFile("And")
    And.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("And")

    assert(dynObj.And(false, false) === false)
    assert(dynObj.And(true, false) === false)
    assert(dynObj.And(false, true) === false)
    assert(dynObj.And(true, true) === true)
  }

  "Or" should "compile and function" in {
    val cf = mkMinimalClassFile("Or")
    Or.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Or")

    assert(dynObj.Or(false, false) === false)
    assert(dynObj.Or(true, false) === true)
    assert(dynObj.Or(false, true) === true)
    assert(dynObj.Or(true, true) === true)
  }

  "Not" should "compile and function" in {
    val cf = mkMinimalClassFile("Not")
    Not.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Not")

    assert(dynObj.Not(true) === false)
    assert(dynObj.Not(false) === true)
  }

  "Implication" should "compile and function" in {
    val cf = mkMinimalClassFile("Implication")
    Implication.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Implication")

    assert(dynObj.Implication(false, false) === true)
    assert(dynObj.Implication(true, false) === false)
    assert(dynObj.Implication(false, true) === true)
    assert(dynObj.Implication(true, true) === true)
  }

  "XOr" should "compile and function" in {
    val cf = mkMinimalClassFile("XOr")
    XOr.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("XOr")

    assert(dynObj.XOr(false, false) === false)
    assert(dynObj.XOr(true, false) === true)
    assert(dynObj.XOr(false, true) === true)
    assert(dynObj.XOr(true, true) === false)
  }

  "Equal" should "compile and function" in {
    val cf = mkMinimalClassFile("Equal")
    Equal.addToClass(cf)

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("Equal")

    assert(dynObj.Equal(false, false) === true)
    assert(dynObj.Equal(true, false) === false)
    assert(dynObj.Equal(false, true) === false)
    assert(dynObj.Equal(true, true) === true)
  }
}
