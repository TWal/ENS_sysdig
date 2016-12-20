
%{
#include <stdlib.h>
void yyerror(const char*);
#include "y.tab.h"

int code_flag(char c) {
    switch(c) {
        case 'z' : return 0;
        case 'c' : return 2;
        case 'p' : return 4;
        case 'o' : return 6;
        case 's' : return 8;
    }
}

int code_test(char* tst) {
    if(strcmp(tst, "eq"))         return 0;
    else if(strcmp(tst, "g"))     return 1;
    else if(strcmp(tst, "ge"))    return 2;
    else if(strcmp(tst, "l"))     return 3;
    else if(strcmp(tst, "le"))    return 4;
    else if(strcmp(tst, "gi"))    return 5;
    else if(strcmp(tst, "gei"))   return 6;
    else if(strcmp(tst, "li"))    return 7;
    else if(strcmp(tst, "lei"))   return 8;
    else if(strcmp(tst, "andz"))  return 9;
    else if(strcmp(tst, "nandz")) return 10;
    else if(strcmp(tst, "xorz"))  return 11;
    else if(strcmp(tst, "nxorz")) return 12;
    else if(strcmp(tst, "eq60"))  return 13;
    else if(strcmp(tst, "neq60")) return 14;
    yyerror("Invalid test code");
}

%}

digit  [0-9]
letter [a-zA-Z]
label  {letter}({letter}|{digit}|_)*

%%

[ \t]      ; /* Skip whitespace */
:          return DOTS;
{digit}+   { yylval.integer = atoi(yytext); return IMMEDIATE; }
\n+        return NEWLINE;
           
rt         { yylval.integer =  0; return REGISTER; }
hi         { yylval.integer =  1; return REGISTER; }
lo         { yylval.integer =  2; return REGISTER; }
a0         { yylval.integer =  3; return REGISTER; }
a1         { yylval.integer =  4; return REGISTER; }
a2         { yylval.integer =  5; return REGISTER; }
a3         { yylval.integer =  6; return REGISTER; }
sp         { yylval.integer =  7; return REGISTER; }
fp         { yylval.integer =  8; return REGISTER; }
r0         { yylval.integer =  9; return REGISTER; }
r1         { yylval.integer = 10; return REGISTER; }
r2         { yylval.integer = 11; return REGISTER; }
r3         { yylval.integer = 12; return REGISTER; }
r4         { yylval.integer = 13; return REGISTER; }
r5         { yylval.integer = 14; return REGISTER; }
r6         { yylval.integer = 15; return REGISTER; }
           
mov        { yylval.integer =  0; return SINS_BIN; }
movs       { yylval.integer =  1; return SINS_BIN; }
add        { yylval.integer =  4; return SINS_BIN; }
addc       { yylval.integer =  5; return SINS_BIN; }
sub        { yylval.integer =  6; return SINS_BIN; }
subc       { yylval.integer =  7; return SINS_BIN; }
and        { yylval.integer =  8; return SINS_BIN; }
or         { yylval.integer =  9; return SINS_BIN; }
xor        { yylval.integer = 10; return SINS_BIN; }
nand       { yylval.integer = 11; return SINS_BIN; }
lsr        { yylval.integer = 12; return SINS_BIN; }
lsl        { yylval.integer = 13; return SINS_BIN; }
asr        { yylval.integer = 14; return SINS_BIN; }
           
not        { yylval.integer =  0; return SINS_UN; }
decr       { yylval.integer =  1; return SINS_UN; }
incr       { yylval.integer =  2; return SINS_UN; }
neg        { yylval.integer =  3; return SINS_UN; }
mulu       { yylval.integer =  4; return SINS_UN; }
muls       { yylval.integer =  5; return SINS_UN; }
divu       { yylval.integer =  6; return SINS_UN; }
divs       { yylval.integer =  7; return SINS_UN; }
lsri       { yylval.integer =  8; return SINS_UN_IMM; }
lsli       { yylval.integer =  9; return SINS_UN_IMM; }
asri       { yylval.integer = 10; return SINS_UN_IMM; }
div10      { yylval.integer = 14; return SINS_UN; }
div10i     { yylval.integer = 15; return SINS_UN; }

js[zcpos]  { yylval.integer = code_flag(yytext[2]);
             return SINS_JFLAG;
           }
jsn[zcpos] { yylval.integer = 1 + code_flag(yytext[2]);
             return SINS_JFLAG;
           }
jseq       { yylval.integer =  0; return SINS_JTEST; }
jsg        { yylval.integer =  1; return SINS_JTEST; }
jsl        { yylval.integer =  2; return SINS_JTEST; }
jsle       { yylval.integer =  3; return SINS_JTEST; }
jsgi       { yylval.integer =  4; return SINS_JTEST; }
jsgei      { yylval.integer =  5; return SINS_JTEST; }
jsli       { yylval.integer =  6; return SINS_JTEST; }
jslei      { yylval.integer =  7; return SINS_JTEST; }
jsandz     { yylval.integer =  8; return SINS_JTEST; }
jsnandz    { yylval.integer =  9; return SINS_JTEST; }
jsxorz     { yylval.integer = 10; return SINS_JTEST; }
jsnxorz    { yylval.integer = 11; return SINS_JTEST; }
jseq60     { yylval.integer = 12; return SINS_JTEST; }
jsneq60    { yylval.integer = 13; return SINS_JTEST; }
jeq        { yylval.integer =  0; return LINS_JTEST; }
jg         { yylval.integer =  1; return LINS_JTEST; }
jl         { yylval.integer =  2; return LINS_JTEST; }
jle        { yylval.integer =  3; return LINS_JTEST; }
jgi        { yylval.integer =  4; return LINS_JTEST; }
jgei       { yylval.integer =  5; return LINS_JTEST; }
jli        { yylval.integer =  6; return LINS_JTEST; }
jlei       { yylval.integer =  7; return LINS_JTEST; }
jandz      { yylval.integer =  8; return LINS_JTEST; }
jnandz     { yylval.integer =  9; return LINS_JTEST; }
jxorz      { yylval.integer = 10; return LINS_JTEST; }
jnxorz     { yylval.integer = 11; return LINS_JTEST; }
jeq60      { yylval.integer = 12; return LINS_JTEST; }
jneq60     { yylval.integer = 13; return LINS_JTEST; }

jmp        return LINS_JMP;
call       return LINS_CALL;
limm       return LINS_LIMM;
j[zcpos]   { yylval.integer = code_flag(yytext[2]);
             return LINS_JFLAG;
           }
jn[zcpos]  { yylval.integer = 1 + code_flag(yytext[2]);
             return LINS_JFLAG;
           }

rdw        { yylval.integer =  0; return INS_MEM_BIN; }
rdbu       { yylval.integer =  1; return INS_MEM_BIN; }
rdbi       { yylval.integer =  2; return INS_MEM_BIN; }
wrw        { yylval.integer =  4; return INS_MEM_BIN; }
wrl        { yylval.integer =  5; return INS_MEM_BIN; }
wrh        { yylval.integer =  6; return INS_MEM_BIN; }
push       { yylval.integer =  8; return INS_MEM_UN; }
pop        { yylval.integer =  9; return INS_MEM_UN; }
ret        { yylval.integer = 10; return INS_MEM; }

{label}    { yylval.string = malloc(yyleng + 1);
             memcpy(yylval.string, yytext, yyleng);
             yylval.string[yyleng] = '\0';
             printf("Found label %s\n", yylval.string);
             return LABEL;
           }

%%

int yywrap(void) {
    return 1;
}

