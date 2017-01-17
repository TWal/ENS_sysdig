all:
	m4 clock.m4 > clock.s
	../asm/asm clock.s -o clock.bin
	m4 clock_fast.m4 > clock_fast.s
	../asm/asm clock_fast.s -o clock_fast.bin
