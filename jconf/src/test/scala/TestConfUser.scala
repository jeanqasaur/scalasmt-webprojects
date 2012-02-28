package test.cap.jeeves.jconf.backend

import cap.jeeves.jconf.backend._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import TestUtil._

class TestConfUser extends FunSuite {
  test ("submitted papers in list") {
    withDataInDatabase {
      expect(true) {
        concretize(
          getContext(author0), author0.getSubmittedPapers().has(paper0.uid)) }
      expect(false) {
        concretize(
          getContext(author0), author0.getSubmittedPapers().has(paper1.uid)) }
    }
  }

  test ("conversion to and from ConfUserRecord") {
    withDataInDatabase {
      val author0Record = author0.getConfUserRecord();
      val author0_copy = author0Record.getConfUser();
      expect(author0.uid) { author0_copy.uid }
      expect(true) {
        concretize(
          getContext(author0)
          , author0.getName() === author0_copy.getName());
      }
      expect(true) {
        concretize(
          getContext(author0)
          , author0_copy.getSubmittedPapers().has(paper0.uid))
      }
      expect(false) {
        concretize(
          getContext(author0)
          , author0_copy.getSubmittedPapers().has(paper1.uid))
      }
    }
  }

  test ("retrieving from the DB") {
    withDataInDatabase {
      val author0_copy =
        JConfTables.getDBConfUser(author0.uid.toInt) match {
          case Some(u) => u
          case None => defaultUser
        }
      expect(true) {
        concretize(
          getContext(author0)
          , author0.getName() === author0_copy.getName());
      }
      expect(true) {
        concretize(
          getContext(author0)
          , author0_copy.getSubmittedPapers().has(paper0.uid))
      } 
      expect(false) {
        concretize(
          getContext(author0)
          , author0_copy.getSubmittedPapers().has(paper1.uid)) 
      }

    }
  }

  test ("self formula") {
    withDataInDatabase {
      expect(true) { concretize[Boolean](getContext(author0), author0.isSelf()) }
      expect(false) {
        concretize[Boolean](getContext(author1), author0.isSelf())
      }
    }
  }
  
  test ("username visibility") {
    withDataInDatabase {
      expect(Username("author0")) {
        show[Username](getContext(author0), author0.username)
      }
      expect(Username("author0")) {
        show[Username](getContext(author1), author0.username)
      }
    }
  }
  
  test ("password visibility") {
    withDataInDatabase {
      expect(Password("a0p")) {
        show[Password](getContext(author0), author0.getPassword ())
      }

      expect(Password("default")) {
        show[Password](getContext(author1), author0.getPassword ())
      }
    }
  } 

  test ("submitted paper visibility - author") {
    withDataInDatabase {
      expect(true) {
        concretize(
          getContext(author0)
          , (author0.getSubmittedPapers ()).has(paper0.uid));
      }
    }
  }

  test ("submitted paper visibility - nonauthor") {
    withDataInDatabase {
      expect(false) {
        concretize(
          getContext(author1), (author0.getSubmittedPapers ()).has(paper0.uid));
      }
    }
  }

  test ("submitted paper exists in list") {
    withDataInDatabase {
      expect(paper0.uid) {
        concretize(getContext(author0), (author0.getSubmittedPapers ()).head);
      }
    }
  }

  test ("submitted paper title") {
    withDataInDatabase {
      expect(paper0Name) {
        author0.showSubmittedPapers (getContext(author0)).head.showTitle(getContext(author0));
      }
    }
  }

  test ("showTitle with login user") {
    withDataInDatabase {
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

  test ("print counts") {
    printPolicyCount()
  }
}
