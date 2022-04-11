import java.io.{FileWriter, File}
import Reg.*

def fib(implicit writer: Writer): Unit =
  data {}
  func("main", true) {
    val arg = A(0)
    val res = V(0)
    arg := 10
    call("fib")
    arg := res
    syscall(1)
    syscall(10)
  }
  func("fib") {
    val arg = A(0)
    val res = V(0)
    val one = T(1)
    val tmp = S(0)
    T(1) := 1
    `if`(arg ~= Zero) {
      res := 0
    } {
      `if`(arg ~= one) {
        res := 1
      } {
        arg := arg - 1
        push(arg)
        call("fib")
        pop(arg)
        tmp := res
        arg := arg - 1
        push(tmp)
        call("fib")
        pop(tmp)
        res := tmp + res
      }
    }
  }

def exec(prog: Writer => Unit, out: String = "out.asm"): Unit =
  val w = new Writer
  prog(w)
  w.acc.foreach(println)
  val fileWriter = new FileWriter(new File(out))
  fileWriter.write(w.acc.mkString("\n"))
  fileWriter.close()

@main def main: Unit =
  exec(fib)
