package test.cap.jeeves.jconf.backend

import cap.scalasmt._
import cap.jeeves.jconf.backend._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import TestUtil._

class TestPaperRecord extends FunSuite {
  test ("sanity check - papers equal themselves") {
    withDataInDatabase {
      expect(paper0) { paper0 }
    }
  }

  test ("title policy") {
    withDataInDatabase {
      expect(StringVal(paper0Name)) {
        show[StringVal](getAuthorCtxt0(Submission), paper0.title)
      }
    }
  }

  test ("object/record conversion") {
    withDataInDatabase {
      val paper0_copy = paper0.getPaperItemRecord().getPaperRecord();
      expect(paper0.uid) {
        paper0_copy.uid
      }
      expect(2) {
        paper0_copy.getAuthors().length
      }
      expect(paper0Name) {
        show[StringVal](
          getAuthorCtxt0(Submission), paper0_copy.title).v;
      }
    }
  }

  test ("context viewer id") {
    withDataInDatabase {
      expect(author0) {
        concretize(getAuthorCtxt0(Submission), CONTEXT.viewer)
      }
      expect(author0.uid) {
        concretize[BigInt](getAuthorCtxt0(Submission), CONTEXT.viewer~'uid)
      }
    }
  }

  test ("isAuthor") {
    withDataInDatabase {
      val isPaper0Author =
        paper0.getAuthors().hasFormula((a: Symbolic) =>
        a~'uid === CONTEXT.viewer~'uid);

      // Sanity check.
      expect(false) {
        concretize( getAuthorCtxt0(Submission)
          , paper0.getAuthors().hasFormula((a: Symbolic) =>
            a.uid === IntVal(-1)) );
      }
      expect(false) {
        concretize( getAuthorCtxt0(Submission)
          , paper0.getAuthors().hasFormula((a: Symbolic) =>
            a.uid === IntVal(author2.uid)) );
      }
      expect(true) {
        concretize(getAuthorCtxt0(Submission), isPaper0Author)
      }
      expect(author2.uid) {
        concretize[BigInt](getAuthorCtxt2(Submission), author2.uid)
      }
      expect(false) {
        concretize(getAuthorCtxt2(Submission), isPaper0Author)
      }
    }
  }

  test ("getAuthors list length") {
    withDataInDatabase {
      expect(2) { paper0.getAuthors().length }
    }
  }

  test ("getAuthors") {
    withDataInDatabase {
      expect(true) {
        concretize(
          getAuthorCtxt0 (Submission)
          , paper0.getAuthors().hasFormula(
            (a: Symbolic) => a~'uid === IntVal(author0.uid)))
      }
    }
  }

  test ("title visibility - author can see title") {
    withDataInDatabase {
      expect(StringVal(paper0Name)) {
        concretize(getAuthorCtxt0 (Submission), paper0.title)
      }
    }
  }

  test ("title visibility - nonauthor cannot see title") {
    withDataInDatabase {
      expect(StringVal("No permission")) {
        concretize(getAuthorCtxt2 (Submission), paper0.title)
      }
    }
  }

  test ("get by ID") {
    withDataInDatabase {
      expect(paper0.uid) {
        getPaperById(paper0.uid.toInt) match {
          case Some(p)  => p.uid
          case None     => -1
        }
      }
    }
  }
}
