#r0 = low 16 bits of timestamp
#r1 = high 16 bits of timestamp
#r2 = year % 4
#r3 = year % 100
#r4 = year % 400
#r5 = saved low
#r6 = saved high
#a0 = 4
#a1 = 100
#a2 = 400
#a3 = isLeap
#lo = nb to subtract from low bits
#hi = nb to subtract from high bits
#fp = the year

main:
    limm 65535 sp
    limm 0 r6
    rdw r6 r0 8
    rdw r6 r1 10
    limm 1969 fp
    limm 1 r2
    limm 69 r3
    limm 369 r4
    limm 4 a0
    limm 100 a1
    limm 400 a2

#calculate isLeap
calcLeap:
    limm 0 a3

    incr fp
#increment r2,r3,r4 and set a3
    incr r2
    jneq r2 a0 leap4
    limm 0 r2
    limm 1 a3
leap4:
    incr r3
    jneq r3 a1 leap100
    limm 0 r3
    limm 0 a3
leap100:
    incr r4
    jneq r4 a2 leap400
    limm 0 r3
    limm 1 a3
leap400:
    mov r0 r5
    mov r1 r6
    jandz a3 a3 isNotLeap

#set hi and lo to the right values
isLeap:
    limm 482 hi
    limm 34048 lo
    jmp subtract
isNotLeap:
    limm 481 hi
    limm 13184 lo

#subtract
subtract:
    mov r0 r5
    mov r1 r6
    sub lo r0
    subc hi r1
    jc endYears
    jmp calcLeap

#Years: done.
endYears:
    mov r5 r0
    mov r6 r1

#r0 = low 16 bits of timestamp
#r1 = high 16 bits of timestamp
#r2 = nb seconds in february high
#r3 = nb seconds in 30 days high
#r4 = nb seconds in 31 days high
#r5 = saved low
#r6 = saved high
#a0 = nb seconds in february low
#a1 = nb seconds in 30 days low
#a2 = nb seconds in 31 days low
#a3 = isLeap then the number of the month
#fp = the year

    jandz a3 a3isNotLeap2
isLeap2:
    limm 59904 a0
    limm 36 r2
    jmp setMagic
isNotLeap2:
    limm 15232 a0
    limm 38 r2
setMagic:
    limm 36096 a1
    limm 39 r3
    limm 56960 a2
    limm 40 r4
    limm 0 a3

define(HANDLEMONTH,
    incr a3
    mov r0 r5
    mov r1 r6
    sub $1 r0
    subc $2 r1
    jc endMonths)
define(HANDLE31, HANDLEMONTH(a2, r4))
define(HANDLE30, HANDLEMONTH(a1, r3))
define(HANDLEFEB, HANDLEMONTH(a0, r2))

HANDLE31  #01
HANDLEFEB #02
HANDLE31  #03
HANDLE30  #04
HANDLE31  #05
HANDLE30  #06
HANDLE31  #07
HANDLE31  #08
HANDLE30  #09
HANDLE31  #10
HANDLE30  #11
HANDLE31  #12

endMonths:
    mov r5 r0
    mov r6 r1

#r0 = low 16 bits of timestamp
#r1 = high 16 bits of timestamp
#r2 = nb seconds in day low
#r3 = nb seconds in day high
#r5 = saved low
#r6 = saved high
#a2 = number of the day

    limm 0 a2
    limm 20864 r2
    limm 1 r3
calcDays:
    incr a2
    mov r0 r5
    mov r1 r6
    sub r2 r0
    subc r3 r1
    jnc calcDays
endDays:

###

    mov r0 lo
    mov r1 hi
    limm 3600 r2
    divu r2
    mov hi a1
    limm 0 hi
    limm 60 r2
    divu r2
    mov hi a0

# years: fp
# month: a3
# day: a2
# hour: a1
# minute : a0
# seconds : lo
