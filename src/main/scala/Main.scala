import Reg.*

def test(implicit writer: Writer): Unit =
  data { static("xs", "word", List(1, 2, 3, 4, 5, -6, -7, -8, -9, -10)) }
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
    syscall(1)
    syscall(10)
  }

  func("abs") {
    `if`(Zero < A(0)) {
      V(0) := A(0)
    } {
      V(0) := Zero - A(0)
    }
  }

def suml(implicit writer: Writer): Unit =
  data { static("xs", "word", List(1, 2, 3, 4, 5, -6, -7, -8, -9, -10)) }
  func("main", true) {
    S(0) := Label("xs")
    T(1) := S(0) + 36
    A(0) := 0
    `while`(S(0) < T(1)) {
      T(0) := S(0).deref()
      S(0) := S(0) + 4
      `if`(T(0) < Zero) {
        continue
      } {
        A(0) := A(0) + T(0)
      }
    }
    syscall(1)
    syscall(10)
  }

@main def hello: Unit =
  val w = new Writer
  suml(w)
  w.acc.foreach(println)
