import Reg.*

def test(implicit writer: Writer): Unit =
  data { static("xs", "word", List(1, 2, 3, 4, 5, -6, -7, -8, -9, -10)) }
  func("main", true) {
    val start = S(1)
    val end = T(2)
    val acc = S(2)
    val tmp = T(1)
    val arg = A(0)

    start := Label("xs")
    acc := 0
    end := start + 40

    `while`(start < end) {
      tmp := start.deref()
      acc := acc + tmp
      start := start + 4
    }

    arg := acc
    call("abs")
    arg := V(0)
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
    // bind registers to make it more readable
    val start = S(0)
    val end = T(1)
    val acc = A(0)
    val tmp = T(0)

    start := Label("xs")
    end := start + 40
    acc := 0
    `while`(start < end) {
      tmp := start.deref()
      start := start + 4
      `if`(tmp < Zero) {
        continue
      } {
        acc := acc + tmp
      }
    }
    syscall(1)
    syscall(10)
  }

@main def hello: Unit =
  val w = new Writer
  test(w)
  w.acc.foreach(println)
