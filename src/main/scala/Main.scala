import Reg.*

def test(implicit writer: Writer): Unit =
  func("main") {
    S(1) := Label("xs")
    S(2) := 0
    T(2) := S(1) + 40

    `while`(S(1) < T(2)) {
      T(1) := S(1).deref()
      S(2) := S(2) + T(1)
      S(1) := S(1) + 4
    }

    A(0) := S(2)
    call("abs")
    A(0) := V(0)
    V(0) := 1
    syscall
    V(0) := 10
    syscall
  }
  
  func("abs") {
    `if`(Zero < A(0)) {
      V(0) := A(0)
    } {
      V(0) := Zero - A(0)
    }
  }

@main def hello: Unit =
  val w = new Writer
  test(w)
  w.acc.foreach(println)
