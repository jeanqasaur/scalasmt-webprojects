package test.cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.jconf._
import cap.jeeves.jconf.JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map
import scala.collection.mutable.Set
import scala.util.Random

import TestUtil._

class TestPaperRecord extends FunSuite {
  test ("title visibility") {
    expect(paper0Name) {
      concretize(getAuthorCtxt0 (), paper0.title)
    }
    expect(Title("No permission")) {
      concretize(getAuthorCtxt2 (), paper0.title)
    }
  }
}
