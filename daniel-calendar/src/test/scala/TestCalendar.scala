package test.cap.scalasmt.webapp

import org.scalatest.FunSuite
import org.scalatest.Assertions

import cap.scalasmt.webapp._

class TestCalendar extends FunSuite {

  test ("dummyVal") {
    val c: Calendar = new Calendar();

    expect("hello") { c.getDummyVal(); }
  }

}
