package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import Expr._

class ExampleSceeves extends FunSuite with Sceeves {
  test ("bool var") {
    val x = pickBool();
    assume (! x);
    expect (false) {concretize(x)};
  }

  /*
  test("symbolic fields") {
    case class Dummy(uid: BigInt) extends Atom
    val x: Dummy = Dummy(0);
    val xVar: ObjectVar[Dummy] = pickObject(x);
    assume(xVar === x);

    val y: Dummy = Dummy(1);
    val yVar: ObjectVar[Dummy] = pickObject(y);
    assume(yVar === y);

    expect(true) { concretize(xVar === xVar) }
    expect(true) { concretize(xVar~'uid === xVar~'uid) }
    expect(false) { concretize(xVar === yVar) }
    expect(false) { concretize(xVar~'uid === yVar~'uid) }
  }
  */
}
