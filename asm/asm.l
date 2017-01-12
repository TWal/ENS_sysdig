

%{
#include<iostream>
#include"asm.hpp"
#include"Scanner.h"

using namespace std;
using namespace yy;
typedef yy::parser::token token;



#define yyerror error


// This code is executed at each action : update position in code.
#define YY_USER_ACTION loc->step(); loc->columns(yyleng);




int code_flag(char c) {
    switch(c) {
        case 'z' : return 0;
        case 'c' : return 2;
        case 'p' : return 4;
        case 'o' : return 6;
        case 's' : return 8;
    }
    cerr << "Impossible to get here in code_flag"<<endl; exit(1);
}

%}

%option c++
%option yyclass="Scanner"
%option yylineno
%option noyywrap


digit  [0-9]
letter [a-zA-Z]
label  {letter}({letter}|{digit}|_)*

%%

[ \t]      ; /* Skip whitespace */
:          return token::DOTS;
{digit}+   { yyval->build( atoi(yytext)); return token::IMMEDIATE; }
\n         {loc->lines() ;return token::NEWLINE;}

rt         { yyval->build(  0); return token::REGISTER; }
hi         { yyval->build(  1); return token::REGISTER; }
lo         { yyval->build(  2); return token::REGISTER; }
a0         { yyval->build(  3); return token::REGISTER; }
a1         { yyval->build(  4); return token::REGISTER; }
a2         { yyval->build(  5); return token::REGISTER; }
a3         { yyval->build(  6); return token::REGISTER; }
sp         { yyval->build(  7); return token::REGISTER; }
fp         { yyval->build(  8); return token::REGISTER; }
r0         { yyval->build(  9); return token::REGISTER; }
r1         { yyval->build( 10); return token::REGISTER; }
r2         { yyval->build( 11); return token::REGISTER; }
r3         { yyval->build( 12); return token::REGISTER; }
r4         { yyval->build( 13); return token::REGISTER; }
r5         { yyval->build( 14); return token::REGISTER; }
r6         { yyval->build( 15); return token::REGISTER; }

mov        { yyval->build(  0); return token::SINS_BIN; }
movs       { yyval->build(  1); return token::SINS_BIN; }
lsr        { yyval->build(  4); return token::SINS_BIN; }
lsl        { yyval->build(  5); return token::SINS_BIN; }
asr        { yyval->build(  6); return token::SINS_BIN; }
add        { yyval->build(  8); return token::SINS_BIN; }
addc       { yyval->build(  9); return token::SINS_BIN; }
sub        { yyval->build( 10); return token::SINS_BIN; }
subc       { yyval->build( 11); return token::SINS_BIN; }
and        { yyval->build( 12); return token::SINS_BIN; }
or         { yyval->build( 13); return token::SINS_BIN; }
xor        { yyval->build( 14); return token::SINS_BIN; }
nand       { yyval->build( 15); return token::SINS_BIN; }

not        { yyval->build(  0); return token::SINS_UN; }
decr       { yyval->build(  1); return token::SINS_UN; }
incr       { yyval->build(  2); return token::SINS_UN; }
neg        { yyval->build(  3); return token::SINS_UN; }
lsri       { yyval->build(  4); return token::SINS_UN_IMM; }
lsli       { yyval->build(  5); return token::SINS_UN_IMM; }
asri       { yyval->build(  6); return token::SINS_UN_IMM; }
mulu       { yyval->build(  8); return token::SINS_UN; }
muls       { yyval->build(  9); return token::SINS_UN; }
divu       { yyval->build( 10); return token::SINS_UN; }
divs       { yyval->build( 11); return token::SINS_UN; }
div10      { yyval->build( 14); return token::SINS_UN; }
div10i     { yyval->build( 15); return token::SINS_UN; }

js[zcpos]  { yyval->build( code_flag(yytext[2]));
             return token::SINS_JFLAG;
           }
jsn[zcpos] { yyval->build( 1 + code_flag(yytext[3]));
             return token::SINS_JFLAG;
           }
jseq       { yyval->build(  0); return token::SINS_JTEST; }
jsg        { yyval->build(  1); return token::SINS_JTEST; }
jsl        { yyval->build(  2); return token::SINS_JTEST; }
jsle       { yyval->build(  3); return token::SINS_JTEST; }
jsgi       { yyval->build(  4); return token::SINS_JTEST; }
jsgei      { yyval->build(  5); return token::SINS_JTEST; }
jsli       { yyval->build(  6); return token::SINS_JTEST; }
jslei      { yyval->build(  7); return token::SINS_JTEST; }
jsandz     { yyval->build(  8); return token::SINS_JTEST; }
jsnandz    { yyval->build(  9); return token::SINS_JTEST; }
jsxorz     { yyval->build( 10); return token::SINS_JTEST; }
jsnxorz    { yyval->build( 11); return token::SINS_JTEST; }
jseq60     { yyval->build( 12); return token::SINS_JTEST; }
jsneq60    { yyval->build( 13); return token::SINS_JTEST; }
jeq        { yyval->build(  0); return token::LINS_JTEST; }
jg         { yyval->build(  1); return token::LINS_JTEST; }
jl         { yyval->build(  2); return token::LINS_JTEST; }
jle        { yyval->build(  3); return token::LINS_JTEST; }
jgi        { yyval->build(  4); return token::LINS_JTEST; }
jgei       { yyval->build(  5); return token::LINS_JTEST; }
jli        { yyval->build(  6); return token::LINS_JTEST; }
jlei       { yyval->build(  7); return token::LINS_JTEST; }
jandz      { yyval->build(  8); return token::LINS_JTEST; }
jnandz     { yyval->build(  9); return token::LINS_JTEST; }
jxorz      { yyval->build( 10); return token::LINS_JTEST; }
jnxorz     { yyval->build( 11); return token::LINS_JTEST; }
jeq60      { yyval->build( 12); return token::LINS_JTEST; }
jneq60     { yyval->build( 13); return token::LINS_JTEST; }

jmp        return token::LINS_JMP;
call       return token::LINS_CALL;
limm       return token::LINS_LIMM;
j[zcpos]   { yyval->build( code_flag(yytext[2]));
             return token::LINS_JFLAG;
           }
jn[zcpos]  { yyval->build( 1 + code_flag(yytext[2]));
             return token::LINS_JFLAG;
           }

rdw        { yyval->build(  0); return token::INS_MEM_BIN; }
rdbu       { yyval->build(  1); return token::INS_MEM_BIN; }
rdbi       { yyval->build(  2); return token::INS_MEM_BIN; }
wrw        { yyval->build(  4); return token::INS_MEM_BIN; }
wrl        { yyval->build(  5); return token::INS_MEM_BIN; }
wrh        { yyval->build(  6); return token::INS_MEM_BIN; }
push       { yyval->build(  8); return token::INS_MEM_UN; }
pop        { yyval->build(  9); return token::INS_MEM_UN; }
ret        { yyval->build( 10); return token::INS_MEM; }

{label}    { yyval->build(std::string(yytext));
             //cout << "Found label :" << yytext << endl;
             return token::LABEL;
           }

%%

int yywrap(void) {
    return 1;
}

