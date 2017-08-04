package state.model

import utest._

import scala.scalajs.js.annotation._
import scala.scalajs.js

@js.native
@JSGlobal("process")
object NodeProcess extends js.Any {
  val version: String = js.native
}

class NameClass {}

/**
  * Testing basic building blocks of the code on the js platform so as to not have surprises later.
  */
object Basics extends TestSuite {
  val tests = this{
    'version{
      println("node.js version is")
      println(NodeProcess.version)
    }
    'getClassName{
      println("class name is ", new NameClass().getClass, new NameClass().getClass.getName)
    }
  }
}