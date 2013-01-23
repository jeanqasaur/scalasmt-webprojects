package test.cap.jeeves.jconf.backend

import cap.scalasmt._
import cap.jeeves.jconf.backend._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import TestUtil._

class TestJConfBackend extends FunSuite {
  /*
  // Name visibility
  test ("name visibility") {
    expect(paper0Name) { concretize(getAuthorCtxt0(), paper0.getTitle ()); }
    expect(emptyName) { concretize(getAuthorCtxt2(), paper0.getTitle ()); }

    val viewMap =
      Map((Submission, paper0Name), (Review, paper0Name), (Decision, paper0Name));
    viewMap.foreach {
      case (stage, r) =>
        expect (r) {
          concretize(getReviewerCtxt0(stage), paper0.getTitle ())
        };
        expect (r) {
          concretize(getPcCtxt0(stage), paper0.getTitle ());
        }
    }

    expect(emptyName) {
      concretize(getPublicCtxt0(Submission), paper1.getTitle ());
    }
    expect(paper1Name) {
      concretize(getPublicCtxt0(Public), paper1.getTitle());
    }
  }

  // Author list visibility
  test ("author list") {
    expect (true) {
      concretize(getAuthorCtxt0(), (paper0.getAuthors ()).has(author0))
    };
    expect (true) {
      concretize(getAuthorCtxt1(), (paper0.getAuthors ()).has(author0))
    }
    expect (false) {
      concretize( getReviewerCtxt0(Submission)
                , (paper0.getAuthors ()).has(author0));
    }
    expect (true) {
      concretize(
        getReviewerCtxt0(Decision), (paper0.getAuthors ()).has(author0));
    }
    expect (true) {
      concretize(getPcCtxt0(Decision), (paper0.getAuthors ()).has(author0));
    }
  }

  test ("tag visibility") {
    expect (false) {
      concretize(getAuthorCtxt0(Decision), paper1.hasTag(Accepted)) }
    expect (true) {
      concretize(getAuthorCtxt0(Public), paper1.hasTag(Accepted));
    }
  }

  test ("tag state change") {
    expect (false) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
    paper0.addTag(Accepted);
    expect (true) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
    paper0.removeTag(Accepted);
    expect (false) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }

  }
  */
  test ("review assignment") {
    expect (true) { isAssigned(paper0, reviewer0); }
    expect (true) { paper0.showNeedsReviewBy(getReviewerCtxt0()) }

    expect (true) { isAssigned(paper0, reviewer1); }
    expect (false) { isAssigned(paper1, reviewer0); }
    expect (false) { isAssigned(paper1, reviewer1); }

    assignReview(paper1, author0);
    expect (false) { isAssigned(paper1, author0); }
  }
  /*
  test ("review tag visibility") {
    expect (false) {
      concretize(getAuthorCtxt0(Review), paper0.hasTag(ReviewedBy(reviewer0)));
    }
    expect (true) {
      concretize(getReviewerCtxt0(Review), paper0.hasTag(ReviewedBy(reviewer0)));
    }
    expect (true) {
      concretize(getPcCtxt0(Review), paper0.hasTag(ReviewedBy(reviewer0)));
    }
  }

  test ("review visibility") {
    expect(new PaperReview()) {
      concretize(getAuthorCtxt1(Review), paper0Review);
    }
    expect(new PaperReview()) {
      concretize(getAuthorCtxt0(Review), paper0Review.getReviewer ());
    }
    expect(reviewer0) {
      concretize(getReviewerCtxt0(Review), paper0Review.getReviewer ());
    }
  }
  */

  test ("get by ID") {
    // TODO: Figure out why the same value doesn't come back out when
    expect(paper0.uid) {
      getPaperById(paper0.uid.toInt) match {
        case Some(p)  => p.uid
        case None     => -1
      }
    }
  }

  /*
  test("paper visibility of rejected paper") {
    expect(false) {
      concretize( getPublicCtxt0(Public)
                , searchByTitle("my paper").has(paper0) );
    }
  }

  test("paper visibility of accepted paper") {
    expect(true) {
      concretize( getPublicCtxt0(Public)
                , searchByTitle("hello world").has(paper1) );
    }
  }
*/
  test ("author visibility") {
    expect(true) {
      concretize( getPublicCtxt0(Public)
                , searchByAuthor(author2).has(paper1) );
    }

  }

  test ("login") {
    expect(Some(author0)) {
      loginUser("author0", "a0p");
    }
    expect(None) {
      loginUser("author0", "xxx");
    }
  }
}
