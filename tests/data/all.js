

function TestFuncDecl(x) {
  switch x{
    case 0:
      this.x = 0;
    default:
      this.x = 10;
  }
}

var testFuncExpr = function() {};

class TestClass {
    throwTest() {
      try {
        let a = --1;
        a++;
      } catch(...) {
        testFuncExpr();
        var x = new TestFuncDecl(0);
      }
    }
}
