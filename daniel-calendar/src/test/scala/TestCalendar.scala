package test.cap.scalasmt.webapp

import org.scalatest.FunSuite
import org.scalatest.Assertions

import cap.scalasmt.webapp._

import java.util._

class TestCalendar extends FunSuite {

/*  test ("dummyVal") {
    val c: Calendar = new Calendar();

    expect("hello") { c.getDummyVal(); }
  }*/

	test("Event toString") {
		val d:Date = new Date(1322503200);
		val e:Event = new Event("UROP Meeting",d,"Group meeting","Stata Center");

		expect("dummyVal") { e.toString(); }
	}
}
