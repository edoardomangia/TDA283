    .section .rodata
    .balign 8
.Lprint_int_fmt:
    .asciz "%d\n"
    .balign 8
.Lprint_double_fmt:
    .asciz "%.1f\n"
    .balign 8
.Lread_int_fmt:
    .asciz "%d"
    .balign 8
.Lread_double_fmt:
    .asciz "%lf"

    .text
    .globl printInt
printInt:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 32
    mv a1, a0
    la a0, .Lprint_int_fmt
    call printf
    ld ra, -8(s0)
    ld s0, -16(s0)
    addi sp, sp, 32
    ret

    .globl printDouble
printDouble:
    addi sp, sp, -16
    sd ra, 8(sp)
    fmv.x.d a1, fa0
    la a0, .Lprint_double_fmt
    call printf
    ld ra, 8(sp)
    addi sp, sp, 16
    ret

    .globl printString
printString:
    addi sp, sp, -16
    sd ra, 8(sp)
    call puts
    ld ra, 8(sp)
    addi sp, sp, 16
    ret

    .globl readInt
readInt:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 32
    addi a1, s0, -20
    la a0, .Lread_int_fmt
    call scanf
    lw a0, -20(s0)
    ld ra, -8(s0)
    ld s0, -16(s0)
    addi sp, sp, 32
    ret

    .globl readDouble
readDouble:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 32
    addi a1, s0, -24
    la a0, .Lread_double_fmt
    call scanf
    fld fa0, -24(s0)
    ld ra, -8(s0)
    ld s0, -16(s0)
    addi sp, sp, 32
    ret
