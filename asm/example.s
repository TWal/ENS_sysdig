    limm 10000 sp
    limm 1 r0
    limm 30 a0
    limm 32 a1
    wrw r0 a0
    wrw r0 a1
    limm 1 a0
    mov a0 a1
    limm 16 r0
while:
    call fun_add
    incr r0
    jns while

stand:
    limm 0 a0
    rdw r1 a1
    wrw a1 a0
    jmp stand

fun_add:
    mov r0 r1
    add r0 r1
    rdw r1 a0
    decr r1
    decr r1
    rdw r1 a1
    add a0 a1
    incr r1
    incr r1
    incr r1
    incr r1
    wrw a1 r1
    mov a1 rt
    ret

mov a0 a0
mov a0 a0
mov a0 a0

