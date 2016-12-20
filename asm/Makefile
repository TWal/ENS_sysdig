
all:
	yacc -d asm.y
	flex asm.l
	gcc lex.yy.c y.tab.c -o comp

