class Writer:
  var acc: List[String] = Nil
  def tell(msg: List[String]): Unit = acc ++= msg

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
    writer.tell(s"sw $r, $this" :: Nil)

  override def toString: String = s"$i($r)"

enum BinOp:
  case Addi(r: Reg, i: Int)
  case Add(r: Reg, e: Reg)
  case Sub(r: Reg, e: Reg)
  case Sll(r: Reg, i: Int)
  case Slti(r: Reg, i: Int)
  case Slt(r: Reg, e: Reg)

  def write(res: Reg)(implicit writer: Writer): Unit = this match
    case Addi(r, i) => writer.tell(s"addi $res, $r, $i" :: Nil)
    case Add(r, i)  => writer.tell(s"add $res, $r, $i" :: Nil)
    case Sub(r, i)  => writer.tell(s"sub $res, $r, $i" :: Nil)
    case Sll(r, i)  => writer.tell(s"sll $res, $r, $i" :: Nil)
    case Slti(r, i) => writer.tell(s"slti $res, $r, $i" :: Nil)
    case Slt(r, i)  => writer.tell(s"slt $res, $r, $i" :: Nil)

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
    case i: Label                  => writer.tell(s"la $this, $i" :: Nil)
    case i: Int                    => writer.tell(s"li $this, $i" :: Nil)
    case i: Reg                    => writer.tell(s"move $this, $i" :: Nil)
    case i: BinOp                  => i.write(this)
    case i: Addr                   => writer.tell(s"lw $this, $i" :: Nil)
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
  writer.tell(s"$start:" :: s"${p.neg.bstr} $x, $y, $exit" :: Nil)
  writer.push(start, exit)
  body
  writer.pop()
  writer.tell(s"j $start" :: Nil)
  writer.tell(s"$exit:" :: Nil)

def break(implicit writer: Writer): Unit =
  val exit = writer.loop.head._2
  writer.tell(s"j $exit" :: Nil)

def continue(implicit writer: Writer): Unit =
  val start = writer.loop.head._1
  writer.tell(s"j $start" :: Nil)

def `if`(cond: (Reg, Cond, Reg))(body: => Unit)(elz: => Unit)(implicit
    writer: Writer
): Unit =
  val skip = Label.get
  val els = Label.get
  val (x, p, y) = cond
  writer.tell(s"${p.neg.bstr} $x, $y, $els" :: Nil)
  body
  writer.tell(s"j $skip" :: Nil)
  writer.tell(s"$els:" :: Nil)
  elz
  writer.tell(s"$skip:" :: Nil)

def func(name: String, global: Boolean = false)(body: => Unit)(implicit
    writer: Writer
): Unit =
  import Reg.*
  if global then writer.tell(s".globl $name" :: Nil) else ()
  writer.tell(s"$name:" :: Nil)
  Sp := Sp - 4
  Sp.deref() := Ra
  body
  Ra := Sp.deref()
  Sp := Sp + 4
  writer.tell("jr $ra" :: Nil)

def call(fun: String)(implicit writer: Writer): Unit =
  writer.tell(s"jal $fun" :: Nil)

def syscall(implicit writer: Writer): Unit = writer.tell("syscall" :: Nil)

def goto(label: String)(implicit writer: Writer): Unit =
  writer.tell(s"j $label" :: Nil)

def jump(cond: (Reg, Cond, Reg), label: String)(implicit writer: Writer): Unit =
  val (x, p, y) = cond
  writer.tell(s"${p.bstr} $x, $y, $label" :: Nil)

def block(label: String)(body: => Unit)(implicit writer: Writer): Unit =
  writer.tell(s"$label:" :: Nil)
  body

def data(dts: List[(String, String, String)])(implicit writer: Writer): Unit =
  writer.tell(".data" :: Nil)
  writer.tell(dts.map { case (label, ty, arr) => s"$label: .$ty $arr" })

def text(implicit writer: Writer): Unit =
  writer.tell(".text" :: Nil)
