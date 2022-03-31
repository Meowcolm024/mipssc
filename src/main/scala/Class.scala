class Writer:
  var acc: List[String] = Nil
  def tell(msg: String, label: Boolean = false): Unit =
    acc ++= List((if label then "" else "  ") ++ msg)

  // stack to store loop labels
  var loop: List[(String, String)] = Nil
  def push(start: String, exit: String): Unit = loop = (start, exit) :: loop
  def pop(): Unit = loop = loop.tail

object Label:
  var idx = 0
  def get: String =
    idx += 1
    s"label_$idx"

case class Label(lbl: String):
  override def toString: String = lbl

case class Addr(r: Reg, i: Int):
  def :=(r: Reg)(implicit writer: Writer): Unit =
    writer.tell(s"sw $r, $this")

  override def toString: String = s"$i($r)"

enum BinOp:
  case Addi(r: Reg, i: Int)
  case Add(r: Reg, e: Reg)
  case Sub(r: Reg, e: Reg)
  case Sll(r: Reg, i: Int)
  case Slti(r: Reg, i: Int)
  case Slt(r: Reg, e: Reg)

  def write(res: Reg)(implicit writer: Writer): Unit = this match
    case Addi(r, i) => writer.tell(s"addi $res, $r, $i")
    case Add(r, i)  => writer.tell(s"add $res, $r, $i")
    case Sub(r, i)  => writer.tell(s"sub $res, $r, $i")
    case Sll(r, i)  => writer.tell(s"sll $res, $r, $i")
    case Slti(r, i) => writer.tell(s"slti $res, $r, $i")
    case Slt(r, i)  => writer.tell(s"slt $res, $r, $i")

enum Reg:
  case Zero
  case S(i: Int)
  case T(i: Int)
  case A(i: Int)
  case V(i: Int)
  case Sp
  case Ra

  override def toString: String = this match
    case Zero                     => "$zero"
    case S(i) if 0 <= i && i <= 7 => "$s" ++ i.toString
    case T(i) if 0 <= i && i <= 7 => "$t" ++ i.toString
    case A(i) if 0 <= i && i <= 1 => "$a" ++ i.toString
    case V(i) if 0 <= i && i <= 1 => "$v" ++ i.toString
    case Sp                       => "$sp"
    case Ra                       => "$ra"
    case _                        => throw Exception("invalid register")

  def :=(i: Any)(implicit writer: Writer): Unit = i match
    case i: Label                  => writer.tell(s"la $this, $i")
    case i: Int                    => writer.tell(s"li $this, $i")
    case i: Reg                    => writer.tell(s"move $this, $i")
    case i: BinOp                  => i.write(this)
    case i: Addr                   => writer.tell(s"lw $this, $i")
    case (x: Reg, Cond.Lt, y: Reg) => BinOp.Slt(x, y).write(this)
    case (x: Reg, Cond.Lt, y: Int) => BinOp.Slti(x, y).write(this)
    case _                         => throw Exception("unknown operation")

  def deref(i: Int = 0): Addr = Addr(this, i)

  def +(i: Reg | Int)(implicit writer: Writer): BinOp = i match
    case i: Int => BinOp.Addi(this, i)
    case i: Reg => BinOp.Add(this, i)

  def -(i: Reg | Int)(implicit writer: Writer): BinOp = i match
    case i: Int => BinOp.Addi(this, -i)
    case i: Reg => BinOp.Sub(this, i)

  def <<(i: Int)(implicit writer: Writer): BinOp = BinOp.Sll(this, i)

  def <[A](r: A): (Reg, Cond, A) = (this, Cond.Lt, r)
  def <=[A](r: A): (Reg, Cond, A) = (this, Cond.Le, r)
  def >[A](r: A): (Reg, Cond, A) = (this, Cond.Gt, r)
  def >=[A](r: A): (Reg, Cond, A) = (this, Cond.Ge, r)
  def /=[A](r: A): (Reg, Cond, A) = (this, Cond.Neq, r)
  def ~=[A](r: A): (Reg, Cond, A) = (this, Cond.Eq, r)

enum Cond:
  case Eq, Neq, Lt, Le, Gt, Ge

  def neg: Cond = this match
    case Eq  => Neq
    case Neq => Eq
    case Lt  => Ge
    case Ge  => Lt
    case Le  => Gt
    case Gt  => Le

  def bstr: String = this match
    case Eq  => "beq"
    case Neq => "bne"
    case Lt  => "blt"
    case Ge  => "bge"
    case Le  => "ble"
    case Gt  => "bgt"

def `while`(cond: (Reg, Cond, Reg))(body: => Unit)(implicit
    writer: Writer
): Unit =
  val start = Label.get
  val exit = Label.get
  val (x, p, y) = cond
  writer.tell(s"$start:", true)
  writer.tell(s"${p.neg.bstr} $x, $y, $exit")
  writer.push(start, exit)
  body
  writer.pop()
  writer.tell(s"j $start")
  writer.tell(s"$exit:", true)

def break(implicit writer: Writer): Unit =
  val exit = writer.loop.head._2
  writer.tell(s"j $exit")

def continue(implicit writer: Writer): Unit =
  val start = writer.loop.head._1
  writer.tell(s"j $start")

def `if`(cond: (Reg, Cond, Reg))(body: => Unit)(elz: => Unit)(implicit
    writer: Writer
): Unit =
  val skip = Label.get
  val els = Label.get
  val (x, p, y) = cond
  writer.tell(s"${p.neg.bstr} $x, $y, $els")
  body
  writer.tell(s"j $skip")
  writer.tell(s"$els:", true)
  elz
  writer.tell(s"$skip:", true)

def func(name: String, global: Boolean = false)(body: => Unit)(implicit
    writer: Writer
): Unit =
  import Reg.*
  if global then writer.tell(s".globl $name", true) else ()
  writer.tell(s"$name:", true)
  Sp := Sp - 4
  Sp.deref() := Ra
  body
  Ra := Sp.deref()
  Sp := Sp + 4
  writer.tell("jr $ra")

def call(fun: String)(implicit writer: Writer): Unit =
  writer.tell(s"jal $fun")

def save(implicit writer: Writer): Unit =
  import Reg.*
  (0 to 7).foreach(i => Sp.deref(-(i + 1) * 4) := S(i))
  Sp := Sp - 32

def restore(implicit writer: Writer): Unit =
  import Reg.*
  Sp := Sp + 32
  (0 to 7).foreach(i => S(i) := Sp.deref(-(i + 1) * 4))

def syscall(code: Int)(implicit writer: Writer): Unit =
  Reg.V(0) := code
  writer.tell("syscall")

def goto(label: String)(implicit writer: Writer): Unit =
  writer.tell(s"j $label")

def jump(cond: (Reg, Cond, Reg), label: String)(implicit writer: Writer): Unit =
  val (x, p, y) = cond
  writer.tell(s"${p.bstr} $x, $y, $label")

def block(label: String)(body: => Unit)(implicit writer: Writer): Unit =
  writer.tell(s"$label:", true)
  body

def data(sts: => Unit)(implicit writer: Writer): Unit =
  writer.tell(".data", true)
  sts
  writer.tell(".text", true)

def static(label: String, ty: String, vs: List[Int] | String)(implicit
    writer: Writer
): Unit = vs match
  case vs: List[Int] => writer.tell(s"$label: .$ty ${vs.mkString(" ")}")
  case vs: String    => writer.tell(s"$label: .$ty \"$vs\"")
