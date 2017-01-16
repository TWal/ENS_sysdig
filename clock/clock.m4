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
    limm 65534 sp
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
    limm 41 r5
    wrl a3 r5 0
    wrw r2 r5 1
    wrw r3 r5 3
    wrw r4 r5 5


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

    jandz a3 a3 isNotLeap2
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
    mov r5 r0
    mov r6 r1
    mov r0 lo
    mov r1 hi
    limm 3600 r2
    #divu r2
    call divu_
    mov hi a1
    limm 0 hi
    limm 60 r2
    #divu r2
    call divu_
    mov hi a0

# years: fp
# month: a3
# day: a2
# hour: a1
# minute : a0
# seconds : lo

    mov fp r0 #year
    mov a3 r1 #month
    mov a2 r2 #day
    mov a1 r3 #hour
    mov a0 r4 #minute
    mov lo r5 #seconds
    limm 0 a0
    limm 24 a1
    limm 12 a2
    rdw a0 r6 8 #low bits of timestamp

#r0: years
#r1: months
#r2: days
#r3: hours
#r4: minutes
#r5: seconds
#r6: last timestamp low bits
# a0: 0
# a1: 24
# a2: 12
# a3: tmp

# Write date to GPU
# NOP are needed between GPU instructions
gpu wy  r0
mov a0 a0
gpu wmo r1
mov a0 a0
gpu wd  r2
mov a0 a0
gpu wwd a0 # We haven't computed the day of the week
mov a0 a0
gpu wh  r3
mov a0 a0
gpu wmi r4
mov a0 a0
gpu ws  r5
mov a0 a0

#RAM: 17+i = nb of day in month i
#     42+2i = year modulo [4, 100, 400][i]
#     41   = isLeap

define(SETFEB,
    rdbu a0 rt 41
    limm 28 a3
    add rt a3
    wrl a3 a0 19
)
define(SETMONTH,
    limm $1 a3
    wrl a3 a0 $2
)
    SETMONTH(31, 18)
    SETFEB()
    SETMONTH(31, 20)
    SETMONTH(30, 21)
    SETMONTH(31, 22)
    SETMONTH(30, 23)
    SETMONTH(31, 24)
    SETMONTH(31, 25)
    SETMONTH(30, 26)
    SETMONTH(31, 27)
    SETMONTH(30, 28)
    SETMONTH(31, 29)

    mov sp fp

mainLoop:
    call waitChange
#seconds
    incr r5
    gpu ws r5
    jneq60 r5 a0 mainLoop
    limm 0 r5
    gpu ws r5
#minutes
    incr r4
    jneq60 r4 a0 mainLoop
    limm 0 r4
    gpu wmi r4
#hours
    incr r3
    gpu wh r3
    jneq r3 a1 mainLoop
    limm 0 r3
    gpu wh r3
#days
    incr r2
    limm 17 a3
    add r1 a3
    rdbu a3 a3
    gpu wd r2
    jge r2 a3 mainLoop
    limm 1 r2
    gpu wd r2
#months
    incr r1
    gpu wmo r1
    jneq r1 a2 mainLoop
    limm 1 r1
    gpu wmo r1
#years
    incr r0
    gpu wy r0
    jmp mainLoop
#ici on utilise que r[1-5] sont à 0
    rdw a0 r1 42
    rdw a0 r2 44
    rdw a0 r3 46

    incr r1
    limm 4 a3
    jneq r1 a3 leap4_2
    limm 0 r1
    limm 1 rt
leap4_2:
    incr r2
    limm 100 a3
    jneq r2 a3 leap100_2
    limm 0 r2
    limm 0 rt
leap100_2:
    incr r3
    limm 400 a3
    jneq r3 a3 leap400_2
    limm 0 r3
    limm 1 rt
leap400_2:
    wrw a0 r1 42
    wrw a0 r2 44
    wrw a0 r3 46
    wrl a0 rt 41
    SETFEB()

    limm 0 r1
    limm 0 r2
    limm 0 r3
    jmp mainLoop


waitChange:
    rdw a0 rt 8
    jeq rt r6 waitChange
    mov rt r6
    ret

divu_:
    limm 0 r5
    limm 0 r6
divuLoop:
    sub r2 lo
    subc r5 hi
    jc divuEndLoop
    incr r6
    jmp divuLoop
divuEndLoop:
    mov r6 hi
    add r2 lo
    ret
