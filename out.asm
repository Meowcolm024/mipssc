.data
.text
.globl main
main:
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  li $a0, 10
  jal fib
  move $a0, $v0
  li $v0, 1
  syscall
  li $v0, 10
  syscall
  lw $ra, 0($sp)
  addi $sp, $sp, 4
  jr $ra
fib:
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  li $t1, 1
  bne $a0, $zero, else_1
  li $v0, 0
  j skip_1
else_1:
  bne $a0, $t1, else_2
  li $v0, 1
  j skip_2
else_2:
  addi $a0, $a0, -1
  addi $sp, $sp, -4
  sw $a0, 0($sp)
  jal fib
  lw $a0, 0($sp)
  addi $sp, $sp, 4
  move $s0, $v0
  addi $a0, $a0, -1
  addi $sp, $sp, -4
  sw $s0, 0($sp)
  jal fib
  lw $s0, 0($sp)
  addi $sp, $sp, 4
  add $v0, $s0, $v0
skip_2:
skip_1:
  lw $ra, 0($sp)
  addi $sp, $sp, 4
  jr $ra