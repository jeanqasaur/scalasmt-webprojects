package test.cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.jconf._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map
import scala.collection.mutable.Set
import scala.util.Random

import TestUtil._

class TestConfUser extends FunSuite {
  test ("default user") {
    expect(new ConfUser()) { new ConfUser() }
  }

  test ("self formula") {
    expect(true) { concretize[Boolean](getContext(author0), author0.isSelf()) }
    expect(false) { concretize[Boolean](getContext(author1), author0.isSelf()) }
  }
  
  test ("username visibility") {
    expect(Username("author0")) {
      show[Username](getContext(author0), author0.username)
    }
    expect(Username("author0")) {
      show[Username](getContext(author1), author0.username)
    }
  }
  
  test ("password visibility") {
    expect(Password("a0p")) {
      show[Password](getContext(author0), author0.getPassword ())
    }

    expect(Password("default")) {
      show[Password](getContext(author1), author0.getPassword ())
    }
  } 

  /*
  test ("submitted paper visibility - author") {
    expect(true) {
      concretize(
        getContext(author0)
        , (author0.getSubmittedPapers ()).hasFormula(p =>
          p~'uid === paper0.uid));
    }
  }
  */

  test ("submitted paper visibility - nonauthor") {
    expect(false) {
      concretize(
        getContext(author1), (author0.getSubmittedPapers ()).hasFormula(p =>
          p~'uid === paper0.uid));
    }
  }

  test ("submitted paper exists in list") {
    expect(paper0.uid) {
      concretize(getContext(author0), (author0.getSubmittedPapers ()).head~'uid);
    }
  }

  /*
  test ("submitted paper title") {
    println("submitted paper title");
    expect(Title(paper0Name)) {
      concretize(
        getContext(author0), ((author0.getSubmittedPapers ()).head).title);
    }
  }
  */

  test ("showTitle with login user") {
    val u = loginUser("author0", "a0p");
    expect("my paper") {
      u match {
        case Some(user) =>
          val ctxt = getContext(user);
          ((author0.showSubmittedPapers (ctxt)).head).showTitle (ctxt);
        case None => ""
      }
    }
  }
}
