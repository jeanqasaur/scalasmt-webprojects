package test.cap.scalasmt.webapp

import org.scalatest.FunSuite
import org.scalatest.Assertions

import cap.scalasmt.webapp._

import java.util._
import java.util._
import java.text._

class TestCalendar extends FunSuite {
	test("Event toString") {
		val format:DateFormat = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT);
		val d:Date = format.parse("Dec 5, 2011 12:00 PM");
		val e:Event = new Event("UROP Meeting",d,"Group meeting","Stata Center");

		expect("UROP Meeting at Mon Dec 05 12:00:00 EST 2011") { e.toString(); }
	}

	test("Event compareTo") {
		val format:DateFormat = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT);
		val d:Date = format.parse("Dec 5, 2011 12:00 PM");
		val e1:Event = new Event("UROP Meeting",d,"Group meeting","Stata Center");
		val e2:Event = new Event("UROP Meeting",d,"Training meeting","Stata Center");

		expect(0) { e1.compareTo(e1); }
		expect(true) { e1.compareTo(e2) < 0 }
	}
}
